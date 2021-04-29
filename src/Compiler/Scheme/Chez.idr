module Compiler.Scheme.Chez

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.Scheme.Common
import Compiler.Separate

import Core.Hash
import Core.Context
import Core.Context.Log
import Core.Directory
import Core.Name
import Core.Options
import Core.TT
import Libraries.Utils.Hex
import Libraries.Utils.Path

import Data.List
import Data.List1
import Data.Maybe
import Libraries.Data.NameMap
import Data.Strings
import Data.Vect

import Idris.Env

import System
import System.Directory
import System.File
import System.Info

%default covering

findChez : IO String
findChez
    = do Nothing <- idrisGetEnv "CHEZ"
            | Just chez => pure chez
         path <- pathLookup ["chez", "chezscheme9.5", "scheme"]
         pure $ fromMaybe "/usr/bin/env scheme" path

-- Given the chez compiler directives, return a list of pairs of:
--   - the library file name
--   - the full absolute path of the library file name, if it's in one
--     of the library paths managed by Idris
-- If it can't be found, we'll assume it's a system library and that chez
-- will thus be able to find it.
findLibs : {auto c : Ref Ctxt Defs} ->
           List String -> Core (List (String, String))
findLibs ds
    = do let libs = mapMaybe (isLib . trim) ds
         traverse locate (nub libs)
  where
    isLib : String -> Maybe String
    isLib d
        = if isPrefixOf "lib" d
             then Just (trim (substr 3 (length d) d))
             else Nothing


escapeString : String -> String
escapeString s = pack $ foldr escape [] $ unpack s
  where
    escape : Char -> List Char -> List Char
    escape '"' cs = '\\' :: '\"' :: cs
    escape '\\' cs = '\\' :: '\\' :: cs
    escape c   cs = c :: cs

schHeader : List String -> List String -> String
schHeader libs compilationUnits = unlines
  [ "(import (chezscheme) (support) "
      ++ unwords ["(" ++ cu ++ ")" | cu <- compilationUnits]
      ++ ")"
  , "(case (machine-type)"
  , "  [(i3le ti3le a6le ta6le) (load-shared-object \"libc.so.6\")]"
  , "  [(i3osx ti3osx a6osx ta6osx) (load-shared-object \"libc.dylib\")]"
  , "  [(i3nt ti3nt a6nt ta6nt) (load-shared-object \"msvcrt.dll\")"
  , "                           (load-shared-object \"ws2_32.dll\")]"
  , "  [else (load-shared-object \"libc.so\")]"
  , unlines ["  (load-shared-object \"" ++ escapeString lib ++ "\")" | lib <- libs]
  , ")"
  ]

schFooter : String
schFooter = "(collect 4)\n(blodwen-run-finalisers)\n"

showChezChar : Char -> String -> String
showChezChar '\\' = ("\\\\" ++)
showChezChar c
   = if c < chr 32 || c > chr 126
        then (("\\x" ++ asHex (cast c) ++ ";") ++)
        else strCons c

showChezString : List Char -> String -> String
showChezString [] = id
showChezString ('"'::cs) = ("\\\"" ++) . showChezString cs
showChezString (c ::cs) = (showChezChar c) . showChezString cs

chezString : String -> String
chezString cs = strCons '"' (showChezString (unpack cs) "\"")

mutual
  tySpec : NamedCExp -> Core String
  -- Primitive types have been converted to names for the purpose of matching
  -- on types
  tySpec (NmCon fc (UN "Int") _ []) = pure "int"
  tySpec (NmCon fc (UN "String") _ []) = pure "string"
  tySpec (NmCon fc (UN "Double") _ []) = pure "double"
  tySpec (NmCon fc (UN "Char") _ []) = pure "char"
  tySpec (NmCon fc (NS _ n) _ [_])
     = cond [(n == UN "Ptr", pure "void*"),
             (n == UN "GCPtr", pure "void*"),
             (n == UN "Buffer", pure "u8*")]
          (throw (GenericMsg fc ("Can't pass argument of type " ++ show n ++ " to foreign function")))
  tySpec (NmCon fc (NS _ n) _ [])
     = cond [(n == UN "Unit", pure "void"),
             (n == UN "AnyPtr", pure "void*"),
             (n == UN "GCAnyPtr", pure "void*")]
          (throw (GenericMsg fc ("Can't pass argument of type " ++ show n ++ " to foreign function")))
  tySpec ty = throw (GenericMsg (getFC ty) ("Can't pass argument of type " ++ show ty ++ " to foreign function"))

  handleRet : String -> String -> String
  handleRet "void" op = op ++ " " ++ mkWorld (schConstructor chezString (UN "") (Just 0) [])
  handleRet _ op = mkWorld op

  getFArgs : NamedCExp -> Core (List (NamedCExp, NamedCExp))
  getFArgs (NmCon fc _ (Just 0) _) = pure []
  getFArgs (NmCon fc _ (Just 1) [ty, val, rest]) = pure $ (ty, val) :: !(getFArgs rest)
  getFArgs arg = throw (GenericMsg (getFC arg) ("Badly formed c call argument list " ++ show arg))

  chezExtPrim : Int -> ExtPrim -> List NamedCExp -> Core String
  chezExtPrim i GetField [NmPrimVal _ (Str s), _, _, struct,
                          NmPrimVal _ (Str fld), _]
      = do structsc <- schExp chezExtPrim chezString 0 struct
           pure $ "(ftype-ref " ++ s ++ " (" ++ fld ++ ") " ++ structsc ++ ")"
  chezExtPrim i GetField [_,_,_,_,_,_]
      = pure "(error \"bad getField\")"
  chezExtPrim i SetField [NmPrimVal _ (Str s), _, _, struct,
                          NmPrimVal _ (Str fld), _, val, world]
      = do structsc <- schExp chezExtPrim chezString 0 struct
           valsc <- schExp chezExtPrim chezString 0 val
           pure $ mkWorld $
              "(ftype-set! " ++ s ++ " (" ++ fld ++ ") " ++ structsc ++
              " " ++ valsc ++ ")"
  chezExtPrim i SetField [_,_,_,_,_,_,_,_]
      = pure "(error \"bad setField\")"
  chezExtPrim i SysCodegen []
      = pure $ "\"chez\""
  chezExtPrim i OnCollect [_, p, c, world]
      = do p' <- schExp chezExtPrim chezString 0 p
           c' <- schExp chezExtPrim chezString 0 c
           pure $ mkWorld $ "(blodwen-register-object " ++ p' ++ " " ++ c' ++ ")"
  chezExtPrim i OnCollectAny [p, c, world]
      = do p' <- schExp chezExtPrim chezString 0 p
           c' <- schExp chezExtPrim chezString 0 c
           pure $ mkWorld $ "(blodwen-register-object " ++ p' ++ " " ++ c' ++ ")"
  chezExtPrim i MakeFuture [_, work]
      = do work' <- schExp chezExtPrim chezString 0 work
           pure $ "(blodwen-make-future " ++ work' ++ ")"
  chezExtPrim i prim args
      = schExtCommon chezExtPrim chezString i prim args

-- Reference label for keeping track of loaded external libraries
data Loaded : Type where

-- Label for noting which struct types are declared
data Structs : Type where

cftySpec : FC -> CFType -> Core String
cftySpec fc CFUnit = pure "void"
cftySpec fc CFInt = pure "int"
cftySpec fc CFUnsigned8 = pure "unsigned-8"
cftySpec fc CFUnsigned16 = pure "unsigned-16"
cftySpec fc CFUnsigned32 = pure "unsigned-32"
cftySpec fc CFUnsigned64 = pure "unsigned-64"
cftySpec fc CFString = pure "string"
cftySpec fc CFDouble = pure "double"
cftySpec fc CFChar = pure "char"
cftySpec fc CFPtr = pure "void*"
cftySpec fc CFGCPtr = pure "void*"
cftySpec fc CFBuffer = pure "u8*"
cftySpec fc (CFFun s t) = pure "void*"
cftySpec fc (CFIORes t) = cftySpec fc t
cftySpec fc (CFStruct n t) = pure $ "(* " ++ n ++ ")"
cftySpec fc t = throw (GenericMsg fc ("Can't pass argument of type " ++ show t ++
                         " to foreign function"))

cCall : {auto c : Ref Ctxt Defs} ->
        {auto l : Ref Loaded (List String)} ->
        String -> FC -> (cfn : String) -> (clib : String) ->
        List (Name, CFType) -> CFType -> Core (String, String)
cCall appdir fc cfn clib args (CFIORes CFGCPtr)
    = throw (GenericMsg fc "Can't return GCPtr from a foreign function")
cCall appdir fc cfn clib args CFGCPtr
    = throw (GenericMsg fc "Can't return GCPtr from a foreign function")
cCall appdir fc cfn clib args (CFIORes CFBuffer)
    = throw (GenericMsg fc "Can't return Buffer from a foreign function")
cCall appdir fc cfn clib args CFBuffer
    = throw (GenericMsg fc "Can't return Buffer from a foreign function")
cCall appdir fc cfn clib args ret
    = do loaded <- get Loaded
         lib <- if clib `elem` loaded
                   then pure ""
                   else do (fname, fullname) <- locate clib
                           copyLib (appdir </> fname, fullname)
                           put Loaded (clib :: loaded)
                           pure $ "(load-shared-object \""
                                    ++ escapeString fname
                                    ++ "\")\n"
         argTypes <- traverse (\a => cftySpec fc (snd a)) args
         retType <- cftySpec fc ret
         let call = "((foreign-procedure #f " ++ show cfn ++ " ("
                      ++ showSep " " argTypes ++ ") " ++ retType ++ ") "
                      ++ showSep " " !(traverse buildArg args) ++ ")"

         pure (lib, case ret of
                         CFIORes _ => handleRet retType call
                         _ => call)
  where
    mkNs : Int -> List CFType -> List (Maybe String)
    mkNs i [] = []
    mkNs i (CFWorld :: xs) = Nothing :: mkNs i xs
    mkNs i (x :: xs) = Just ("cb" ++ show i) :: mkNs (i + 1) xs

    applyLams : String -> List (Maybe String) -> String
    applyLams n [] = n
    applyLams n (Nothing :: as) = applyLams ("(" ++ n ++ " #f)") as
    applyLams n (Just a :: as) = applyLams ("(" ++ n ++ " " ++ a ++ ")") as

    getVal : String -> String
    getVal str = "(vector-ref " ++ str ++ "1)"

    mkFun : List CFType -> CFType -> String -> String
    mkFun args ret n
        = let argns = mkNs 0 args in
              "(lambda (" ++ showSep " " (mapMaybe id argns) ++ ") " ++
              (applyLams n argns ++ ")")

    notWorld : CFType -> Bool
    notWorld CFWorld = False
    notWorld _ = True

    callback : String -> List CFType -> CFType -> Core String
    callback n args (CFFun s t) = callback n (s :: args) t
    callback n args_rev retty
        = do let args = reverse args_rev
             argTypes <- traverse (cftySpec fc) (filter notWorld args)
             retType <- cftySpec fc retty
             pure $
                 "(let ([c-code (foreign-callable #f " ++
                       mkFun args retty n ++
                       " (" ++ showSep " " argTypes ++ ") " ++ retType ++ ")])" ++
                       " (lock-object c-code) (foreign-callable-entry-point c-code))"

    buildArg : (Name, CFType) -> Core String
    buildArg (n, CFFun s t) = callback (schName n) [s] t
    buildArg (n, CFGCPtr) = pure $ "(car " ++ schName n ++ ")"
    buildArg (n, _) = pure $ schName n

schemeCall : FC -> (sfn : String) ->
             List Name -> CFType -> Core String
schemeCall fc sfn argns ret
    = let call = "(" ++ sfn ++ " " ++ showSep " " (map schName argns) ++ ")" in
          case ret of
               CFIORes _ => pure $ mkWorld call
               _ => pure call

-- Use a calling convention to compile a foreign def.
-- Returns any preamble needed for loading libraries, and the body of the
-- function call.
useCC : {auto c : Ref Ctxt Defs} ->
        {auto l : Ref Loaded (List String)} ->
        String -> FC -> List String -> List (Name, CFType) -> CFType -> Core (String, String)
useCC appdir fc [] args ret = throw (NoForeignCC fc)
useCC appdir fc (cc :: ccs) args ret
    = case parseCC cc of
           Nothing => useCC appdir fc ccs args ret
           Just ("scheme,chez", [sfn]) =>
               do body <- schemeCall fc sfn (map fst args) ret
                  pure ("", body)
           Just ("scheme", [sfn]) =>
               do body <- schemeCall fc sfn (map fst args) ret
                  pure ("", body)
           Just ("C", [cfn, clib]) => cCall appdir fc cfn clib args ret
           Just ("C", [cfn, clib, chdr]) => cCall appdir fc cfn clib args ret
           _ => useCC appdir fc ccs args ret

-- For every foreign arg type, return a name, and whether to pass it to the
-- foreign call (we don't pass '%World')
mkArgs : Int -> List CFType -> List (Name, Bool)
mkArgs i [] = []
mkArgs i (CFWorld :: cs) = (MN "farg" i, False) :: mkArgs i cs
mkArgs i (c :: cs) = (MN "farg" i, True) :: mkArgs (i + 1) cs

mkStruct : {auto s : Ref Structs (List String)} ->
           CFType -> Core String
mkStruct (CFStruct n flds)
    = do defs <- traverse mkStruct (map snd flds)
         strs <- get Structs
         if n `elem` strs
            then pure (concat defs)
            else do put Structs (n :: strs)
                    pure $ concat defs ++ "(define-ftype " ++ n ++ " (struct\n\t"
                           ++ showSep "\n\t" !(traverse showFld flds) ++ "))\n"
  where
    showFld : (String, CFType) -> Core String
    showFld (n, ty) = pure $ "[" ++ n ++ " " ++ !(cftySpec emptyFC ty) ++ "]"
mkStruct (CFIORes t) = mkStruct t
mkStruct (CFFun a b) = do ignore (mkStruct a); mkStruct b
mkStruct _ = pure ""

schFgnDef : {auto c : Ref Ctxt Defs} ->
            {auto l : Ref Loaded (List String)} ->
            {auto s : Ref Structs (List String)} ->
            String -> FC -> Name -> NamedDef -> Core (String, String)
schFgnDef appdir fc n (MkNmForeign cs args ret)
    = do let argns = mkArgs 0 args
         let allargns = map fst argns
         let useargns = map fst (filter snd argns)
         argStrs <- traverse mkStruct args
         retStr <- mkStruct ret
         (load, body) <- useCC appdir fc cs (zip useargns args) ret
         defs <- get Ctxt
         pure (load,
                concat argStrs ++ retStr ++
                "(define " ++ schName !(full (gamma defs) n) ++
                " (lambda (" ++ showSep " " (map schName allargns) ++ ") " ++
                body ++ "))\n")
schFgnDef _ _ _ _ = pure ("", "")

getFgnCall : {auto c : Ref Ctxt Defs} ->
             {auto l : Ref Loaded (List String)} ->
             {auto s : Ref Structs (List String)} ->
             String -> (Name, FC, NamedDef) -> Core (String, String)
getFgnCall appdir (n, fc, d) = schFgnDef appdir fc n d

startChez : String -> String -> String -> String
startChez chez appDirSh targetSh = unlines
    [ "#!/bin/sh"
    , ""
    , "set -e # exit on any error"
    , ""
    , "case $(uname -s) in            "
    , "    OpenBSD | FreeBSD | NetBSD)"
    , "        REALPATH=\"grealpath\" "
    , "        ;;                     "
    , "                               "
    , "    *)                         "
    , "        REALPATH=\"realpath\"  "
    , "        ;;                     "
    , "esac                           "
    , ""
    , "if ! command -v \"$REALPATH\" >/dev/null; then             "
    , "    echo \"$REALPATH is required for Chez code generator.\""
    , "    exit 1                                                 "
    , "fi                                                         "
    , ""
    , "DIR=$(dirname \"$($REALPATH \"$0\")\")"
    , "export LD_LIBRARY_PATH=\"$DIR/" ++ appDirSh ++ "\":$LD_LIBRARY_PATH"
    , "\"" ++ chez ++ "\" -q "
        ++ "--libdirs \"$DIR/" ++ appDirSh ++ "\" "
        ++ "--program \"$DIR/" ++ targetSh ++ "\" "
        ++ "\"$@\""
    ]

startChezCmd : String -> String -> String -> String
startChezCmd chez appDirSh targetSh = unlines
    [ "@echo off"
    , "set APPDIR=%~dp0"
    , "set PATH=%APPDIR%\\" ++ appDirSh ++ ";%PATH%"
    , "\"" ++ chez ++ "\" -q "
        ++ "--libdirs \"%APPDIR%/" ++ appDirSh ++ "\" "
        ++ "--program \"%APPDIR%/" ++ targetSh ++ "\" "
        ++ "%*"
    ]

startChezWinSh : String -> String -> String -> String
startChezWinSh chez appDirSh targetSh = unlines
    [ "#!/bin/sh"
    , ""
    , "set -e # exit on any error"
    , ""
    , "DIR=$(dirname \"$(realpath \"$0\")\")"
    , "CHEZ=$(cygpath \"" ++ chez ++"\")"
    , "export PATH=\"$DIR/" ++ appDirSh ++ "\":$PATH"
    , "\"$CHEZ\" --program \"$DIR/" ++ targetSh ++ "\" \"$@\""
    , "\"$CHEZ\" -q "
        ++ "--libdirs \"$DIR/" ++ appDirSh ++ "\" "
        ++ "--program \"$DIR/" ++ targetSh ++ "\" "
        ++ "\"$@\""
    ]

compileChezLibrary : (chez : String) -> (libDir : String) -> (ssFile : String) -> Core ()
compileChezLibrary chez libDir ssFile = ignore $ coreLift $ system $ unwords
  [ "echo"
  , "'(parameterize ([optimize-level 3] [compile-file-message #f]) (compile-library " ++ show ssFile ++ "))'"
  , "|", chez, "-q", "--libdirs", libDir
  ]

compileChezProgram : (chez : String) -> (libDir : String) -> (ssFile : String) -> Core ()
compileChezProgram chez libDir ssFile = ignore $ coreLift $ system $ unwords
  [ "echo"
  , "'(parameterize ([optimize-level 3] [compile-file-message #f]) (compile-program " ++ show ssFile ++ "))'"
  , "|", chez, "-q", "--libdirs", libDir
  ]

writeFileCore : (fname : String) -> (content : String) -> Core ()
writeFileCore fname content =
  coreLift (writeFile fname content) >>= \case
    Right () => pure ()
    Left err => throw $ FileErr fname err

readFileCore : (fname : String) -> Core String
readFileCore fname =
  coreLift (readFile fname) >>= \case
    Right content => pure content
    Left err => throw $ FileErr fname err

chezNS : String -> String
chezNS "" = "unqualified"
chezNS ns = ns

-- arbitrarily name the compilation unit
-- after the alphabetically first namespace contained within
chezLibraryName : CompilationUnit def -> String
chezLibraryName cu =
  case cu.namespaces of
    [] => "unknown"  -- this will never happen because the Tarjan algorithm won't produce an empty SCC
    ns::_ => chezNS ns

record ChezLib where
  constructor MkChezLib
  name : String
  isOutdated : Bool  -- needs recompiling

-- if this becomes too inefficient, we may make it tail-recursive
forState : List a -> st -> (st -> a -> Core (st, b)) -> Core (List b)
forState [] st f = pure []
forState (x :: xs) st f = do
  (st', y) <- f st x
  ys <- forState xs st' f
  pure (y :: ys)

||| Compile a TT expression to a bunch of Chez Scheme files
compileToSS : Ref Ctxt Defs -> String -> String -> ClosedTerm -> Core (Bool, List ChezLib)
compileToSS c chez appdir tm = do
  coreLift $ putStrLn "A"
  -- process native libraries
  ds <- getDirectives Chez
  libs <- findLibs ds
  traverse_ copyLib libs

  coreLift $ putStrLn "B"
  -- get the material for compilation
  cdata <- getCompileData False Cases tm
  let ctm = forget (mainExpr cdata)
  let ndefs = namedDefs cdata
  coreLift $ putStrLn "C"
  let cui = getCompilationUnits ndefs
  coreLift $ putStrLn "D"

  -- copy the support library
  support <- readDataFile "chez/support.ss"
  let supportHash = show $ hash support
  supportChanged <-
    coreLift (readFile (appdir </> "support.hash")) >>= \case
      Left err => pure True
      Right fileHash => pure (fileHash /= supportHash)
  when supportChanged $ do
    writeFileCore (appdir </> "support.ss") support
    writeFileCore (appdir </> "support.hash") supportHash

  coreLift $ putStrLn "E"
  -- TODO: add extraRuntime
  -- the problem with this is that it's unclear what to put in the (export) clause of the library
  -- extraRuntime <- getExtraRuntime ds

  -- for each compilation unit, generate code
  chezLibs <- forState cui.compilationUnits SortedSet.empty $ \outdatedCuids, cu => do
    let chezLib = chezLibraryName cu
    coreLift $ putStrLn $ "F " ++ chezLib

    -- run this only if the hash has changed
    let cuHash = show (hash cu)
    hashChanged <-
      coreLift (readFile (appdir </> chezLib <.> "hash")) >>= \case
        Left err       => pure True
        Right fileHash => pure (fileHash /= cuHash)
    let depsOutdated = not $ null (SortedSet.intersection outdatedCuids cu.dependencies)
    let isOutdated = hashChanged || depsOutdated || supportChanged

    when isOutdated $ do
      -- initialise context
      defs <- get Ctxt
      l <- newRef {t = List String} Loaded ["libc", "libc 6"]
      s <- newRef {t = List String} Structs []

      -- create imports + exports + header + footer
      let imports = unwords
            [ "(" ++
                maybe
                  "unqualified"
                  chezLibraryName
                  (SortedMap.lookup cuid cui.byId)
              ++ ")"
            | cuid <- SortedSet.toList cu.dependencies
            ]
      let exports = unwords $ concat
            -- export only the defs that generate Scheme definitions
            [ case d of
                MkNmFun args exp => [schName dn]
                MkNmError exp => [schName dn]
                MkNmForeign _ _ _ => []
                MkNmCon _ _ _ => []
            | (dn, fc, d) <- cu.definitions
            ]
      let header =
            "(library (" ++ chezLib ++ ")\n"
            ++ "  (export " ++ exports ++ ")\n"
            ++ "  (import (chezscheme) (support) " ++ imports ++ ")\n\n"
      let footer = ")"

      fgndefs <- traverse (getFgnCall appdir) cu.definitions
      compdefs <- traverse (getScheme chezExtPrim chezString) cu.definitions

      -- write the files
      log "compiler.scheme.chez" 3 $ "Generating code for " ++ chezLib
      writeFileCore (appdir </> chezLib <.> "ss") $ fastAppend $
        [header]
        ++ map snd fgndefs  -- definitions using foreign libs
        ++ compdefs
        ++ map fst fgndefs  -- foreign library load statements
        ++ [footer]

      writeFileCore (appdir </> chezLib <.> "hash") cuHash

    pure
      ( if isOutdated
          then SortedSet.insert cu.id outdatedCuids
          else outdatedCuids
      , MkChezLib chezLib isOutdated
      )

  -- main module
  main <- schExp chezExtPrim chezString 0 ctm
  writeFileCore (appdir </> "mainprog.ss") $ unlines $
    [ schHeader (map snd libs) [lib.name | lib <- chezLibs]
    , "(collect-request-handler (lambda () (collect) (blodwen-run-finalisers)))"
    , main
    , schFooter
    ]

  pure (supportChanged, chezLibs)

makeSh : String -> String -> String -> String -> Core ()
makeSh chez outShRel appDirSh targetSh
    = do Right () <- coreLift $ writeFile outShRel (startChez chez appDirSh targetSh)
            | Left err => throw (FileErr outShRel err)
         pure ()

||| Make Windows start scripts, one for bash environments and one batch file
makeShWindows : String -> String -> String -> String -> Core ()
makeShWindows chez outShRel appDirSh targetSh
    = do let cmdFile = outShRel ++ ".cmd"
         Right () <- coreLift $ writeFile cmdFile (startChezCmd chez appDirSh targetSh)
            | Left err => throw (FileErr cmdFile err)
         Right () <- coreLift $ writeFile outShRel (startChezWinSh chez appDirSh targetSh)
            | Left err => throw (FileErr outShRel err)
         pure ()

||| Chez Scheme implementation of the `compileExpr` interface.
compileExpr : Bool -> Ref Ctxt Defs -> (tmpDir : String) -> (outputDir : String) ->
              ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr makeitso c tmpDir outputDir tm outfile = do
  -- set up paths
  Just cwd <- coreLift currentDir
       | Nothing => throw (InternalError "Can't get current directory")
  let appDirSh  = outfile ++ "_sep"  -- relative to the launcher shell script
  let appDirRel = outputDir </> appDirSh  -- relative to CWD
  let appDirAbs = cwd </> appDirRel
  coreLift_ $ mkdirAll appDirRel

  -- generate the code
  chez <- coreLift $ findChez
  (supportChanged, chezLibs) <- compileToSS c chez appDirRel tm

  -- compile the code
  logTime "++ Make SO" $ when makeitso $ do
    -- compile the support code
    when supportChanged $ do
      log "compiler.scheme.chez" 3 $ "Compiling support"
      compileChezLibrary chez appDirRel (appDirRel </> "support.ss")

    -- compile every compilation unit
    for_ chezLibs $ \lib =>
      when lib.isOutdated $ do
        log "compiler.scheme.chez" 3 $ "Compiling " ++ lib.name
        compileChezLibrary chez appDirRel (appDirRel </> lib.name <.> "ss")

    -- compile the main program
    compileChezProgram chez appDirRel (appDirRel </> "mainprog.ss")

  -- generate the launch script
  let outShRel = outputDir </> outfile
  let launchTargetSh = appDirSh </> "mainprog" <.> (if makeitso then "so" else "ss")
  if isWindows
     then makeShWindows chez outShRel appDirSh launchTargetSh
     else makeSh        chez outShRel appDirSh launchTargetSh
  coreLift_ $ chmodRaw outShRel 0o755
  pure (Just outShRel)

||| Chez Scheme implementation of the `executeExpr` interface.
||| This implementation simply runs the usual compiler, saving it to a temp file, then interpreting it.
executeExpr : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
executeExpr c tmpDir tm
    = do Just sh <- compileExpr False c tmpDir tmpDir tm "_tmpchez"
            | Nothing => throw (InternalError "compileExpr returned Nothing")
         coreLift_ $ system sh
         pure ()

||| Codegen wrapper for Chez scheme implementation.
export
codegenChez : Codegen
codegenChez = MkCG (compileExpr True) executeExpr
