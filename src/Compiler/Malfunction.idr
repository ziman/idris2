module Compiler.Malfunction

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.Scheme.Common

import Core.Context
import Core.Directory
import Core.Name
import Core.Options
import Core.TT
import Utils.Hex
import Utils.Path
import Utils.Pretty

import Data.List
import Data.Maybe
import Data.NameMap
import Data.Strings
import Data.Vect
import Data.SortedSet

import System
import System.Directory
import System.File
import System.Info

%default covering

debug : Bool
debug = True

heXX : Int -> String
heXX x = hd (x `div` 16) ++ hd (x `mod` 16)
  where
    hd : Int -> String
    hd 10 = "A"
    hd 11 = "B"
    hd 12 = "C"
    hd 13 = "D"
    hd 14 = "E"
    hd 15 = "F"
    hd i = show i

showChar : Char -> String -> String
showChar '\\' = ("\\\\" ++)
showChar '"' = ("\\\"" ++)
showChar '\n' = ("\\n" ++)
showChar c
   = if c < chr 32
        then (("\\x" ++ heXX (cast c) ++ "") ++)
        else strCons c

showString : List Char -> String -> String
showString [] = id
showString (c::cs) = showChar c . showString cs

mlfString : String -> Doc
mlfString cs = text $ strCons '"' (showString (unpack cs) "\"")

sexp : List Doc -> Doc
sexp = parens . hsep

mlfGlobal : String -> Doc
mlfGlobal mlName = parens $
  text "global"
    <++> hsep
      [ text ("$" ++ n)
      | n <- split (== '.') mlName
      ]

mlfApply : Doc -> List Doc -> Doc
mlfApply f [] = f
mlfApply f args = parens $
  text "apply" <++> f
  $$ indentBlock args

mlfLibCall : String -> List Doc -> Doc
mlfLibCall fn args = mlfApply (mlfGlobal fn) args

mlfError : String -> Doc
mlfError msg = mlfLibCall "Stdlib.failwith" [mlfString msg]

mlfDebug : Show a => a -> Doc
mlfDebug = mlfError . show

sanitise : String -> String
sanitise = pack . concatMap sanitise' . unpack
  where
    san : Char -> Bool
    san c =
        ('A' <= c && c <= 'Z')
        || ('a' <= c && c <= 'z')
        || ('0' <= c && c <= '9')

    sanitise' : Char -> List Char
    sanitise' c =
      if san c
        then [c]
        else '-' :: unpack (show $ ord c) ++ ['-']

mlfName : Name -> Doc
mlfName (MN n i) = text (sanitise n) <+> show i
mlfName n = text . sanitise . schName $ n

mlfVar : Name -> Doc
mlfVar n = text "$" <+> mlfName n

mlfLet : Name -> Doc -> Doc -> Doc
mlfLet n val rhs = parens $
  text "let"
  $$ indentBlock
    [ sexp [mlfVar n, val]
    , rhs
    ]

mlfLazy : Doc -> Doc
mlfLazy doc = sexp [text "lazy", doc]

mlfLam : List Name -> Doc -> Doc
mlfLam [] rhs = mlfLazy rhs
mlfLam args rhs =
  parens $
    text "lambda" <++> sexp (map mlfVar args)
    $$ indent rhs

mlfForce : Doc -> Doc
mlfForce doc = sexp [text "force", doc]

mlfBlock : Maybe Int -> List Doc -> Doc
mlfBlock Nothing args = mlfError "no constructor tag (1)"
mlfBlock (Just tag) args = parens $
  text "block" <++> sexp [text "tag", show tag]
  $$ indentBlock args

mlfCmp : String -> String -> String -> List Doc -> Doc
mlfCmp conv cmp zero args = sexp [text cmp, mlfLibCall conv args, text zero]

mlfOp : PrimFn arity -> Vect arity Doc -> Doc
mlfOp (Add IntType) [x,y] = sexp [text "+.int", x,y]
mlfOp (Sub IntType) [x,y] = sexp [text "-.int", x,y]
mlfOp (Mul IntType) [x,y] = sexp [text "*.int", x,y]
mlfOp (Div IntType) [x,y] = sexp [text "/.int", x,y]

mlfOp (LT IntType) [x,y] = sexp [text "<.int", x,y]
mlfOp (LTE IntType) [x,y] = sexp [text "<=.int", x,y]
mlfOp (EQ IntType) [x,y] = sexp [text "==.int", x,y]
mlfOp (GTE IntType) [x,y] = sexp [text ">=.int", x,y]
mlfOp (GT IntType) [x,y] = sexp [text ">.int", x,y]

mlfOp (LT CharType) [x,y] = sexp [text "<.int", x,y]
mlfOp (LTE CharType) [x,y] = sexp [text "<=.int", x,y]
mlfOp (EQ CharType) [x,y] = sexp [text "==.int", x,y]
mlfOp (GTE CharType) [x,y] = sexp [text ">=.int", x,y]
mlfOp (GT CharType) [x,y] = sexp [text ">.int", x,y]

mlfOp (Add IntegerType) [x,y] = sexp [text "+.ibig", x,y]
mlfOp (Sub IntegerType) [x,y] = sexp [text "-.ibig", x,y]
mlfOp (Mul IntegerType) [x,y] = sexp [text "*.ibig", x,y]
mlfOp (Div IntegerType) [x,y] = sexp [text "/.ibig", x,y]

mlfOp (LT IntegerType) [x,y] = sexp [text "<.ibig", x,y]
mlfOp (LTE IntegerType) [x,y] = sexp [text "<=.ibig", x,y]
mlfOp (EQ IntegerType) [x,y] = sexp [text "==.ibig", x,y]
mlfOp (GTE IntegerType) [x,y] = sexp [text ">=.ibig", x,y]
mlfOp (GT IntegerType) [x,y] = sexp [text ">.ibig", x,y]

mlfOp (Cast IntegerType IntType) [x] = sexp [text "convert.ibig.int", x]
mlfOp (Cast IntType IntegerType) [x] = sexp [text "convert.int.ibig", x]
mlfOp (Cast IntegerType CharType) [x] = sexp [text "convert.ibig.int", x]
mlfOp (Cast CharType IntegerType) [x] = sexp [text "convert.int.ibig", x]
mlfOp (Cast CharType IntType) [x] = x
mlfOp (Cast IntType CharType) [x] = x

mlfOp (Cast IntegerType StringType) [x] = mlfLibCall "Z.to_string" [x]
mlfOp (Cast IntType StringType) [x] = mlfLibCall "Stdlib.string_of_int" [x]

mlfOp StrLength [x] = mlfLibCall "Rts.String.length" [x]
mlfOp StrHead [x] = mlfLibCall "Rts.String.head" [x]
mlfOp StrTail [x] = mlfLibCall "Rts.String.tail" [x]
mlfOp StrIndex [x, i] = mlfLibCall "Rts.String.get" [x, i]
mlfOp StrCons [x, xs] = mlfLibCall "Rts.String.cons" [x, xs]
mlfOp StrReverse [x] = mlfLibCall "Rts.String.reverse" [x]
mlfOp StrSubstr [off, len, s] = mlfLibCall "Rts.String.sub" [off, len, s]
mlfOp StrAppend [x,y] = mlfLibCall "Bytes.cat" [x, y]

mlfOp (LT StringType) [x,y] = mlfCmp "String.compare" "<.int" "0" [x,y]
mlfOp (LTE StringType) [x,y] = mlfCmp "String.compare" "<=.int" "0" [x,y]
mlfOp (EQ StringType) [x,y] = mlfCmp "String.compare" "==.int" "0" [x,y]
mlfOp (GTE StringType) [x,y] = mlfCmp "String.compare" ">=.int" "0" [x,y]
mlfOp (GT StringType) [x,y] = mlfCmp "String.compare" ">.int" "0" [x,y]

mlfOp Crash [_, msg] = mlfLibCall "Stdlib.failwith" [msg]
mlfOp BelieveMe [_, _, x] = x

mlfOp op args = mlfError $ "unimplemented primop: " ++ show op

{-
mlfOp (Add ty) = ?rhsOp_1
mlfOp (Sub ty) = ?rhsOp_2
mlfOp (Mul ty) = ?rhsOp_3
mlfOp (Div ty) = ?rhsOp_4
mlfOp (Mod ty) = ?rhsOp_5
mlfOp (Neg ty) = ?rhsOp_6
mlfOp (ShiftL ty) = ?rhsOp_7
mlfOp (ShiftR ty) = ?rhsOp_8
mlfOp (BAnd ty) = ?rhsOp_9
mlfOp (BOr ty) = ?rhsOp_10
mlfOp (BXOr ty) = ?rhsOp_11
mlfOp (LT ty) = ?rhsOp_12
mlfOp (LTE ty) = ?rhsOp_13
mlfOp (EQ ty) = ?rhsOp_14
mlfOp (GTE ty) = ?rhsOp_15
mlfOp (GT ty) = ?rhsOp_16
mlfOp StrLength = ?rhsOp_17
mlfOp StrHead = ?rhsOp_18
mlfOp StrTail = ?rhsOp_19
mlfOp StrIndex = ?rhsOp_20
mlfOp StrCons = ?rhsOp_21
mlfOp StrAppend = ?rhsOp_22
mlfOp StrReverse = ?rhsOp_23
mlfOp StrSubstr = ?rhsOp_24
mlfOp DoubleExp = ?rhsOp_25
mlfOp DoubleLog = ?rhsOp_26
mlfOp DoubleSin = ?rhsOp_27
mlfOp DoubleCos = ?rhsOp_28
mlfOp DoubleTan = ?rhsOp_29
mlfOp DoubleASin = ?rhsOp_30
mlfOp DoubleACos = ?rhsOp_31
mlfOp DoubleATan = ?rhsOp_32
mlfOp DoubleSqrt = ?rhsOp_33
mlfOp DoubleFloor = ?rhsOp_34
mlfOp DoubleCeiling = ?rhsOp_35
mlfOp (Cast x y) = ?rhsOp_36
mlfOp BelieveMe = ?rhsOp_37
mlfOp Crash = ?rhsOp_38
-}

mlfExtPrim : Name -> Doc
mlfExtPrim (NS _ (UN "prim__newArray")) =
  mlfLam [UN "_ty", UN "n", UN "x", UN "_world"] $
    sexp [text "makevec", mlfVar (UN "n"), mlfVar (UN "x")]
mlfExtPrim (NS _ (UN "prim__arrayGet")) =
  mlfLam [UN "_ty", UN "arr", UN "i", UN "_world"] $
    sexp [text "load", mlfVar (UN "arr"), mlfVar (UN "i")]
mlfExtPrim (NS _ (UN "prim__arraySet")) =
  mlfLam [UN "_ty", UN "arr", UN "i", UN "x", UN "_world"] $
    sexp [text "store", mlfVar (UN "arr"), mlfVar (UN "i"), mlfVar (UN "x")]
mlfExtPrim (NS _ (UN "prim__newIORef")) =
  mlfLam [UN "_ty", UN "x", UN "_world"] $
    sexp [text "makevec", show 1, mlfVar (UN "x")]
mlfExtPrim (NS _ (UN "prim__readIORef")) =
  mlfLam [UN "_ty", UN "ref", UN "_world"] $
    sexp [text "load", mlfVar (UN "ref"), show 0]
mlfExtPrim (NS _ (UN "prim__writeIORef")) =
  mlfLam [UN "_ty", UN "ref", UN "x", UN "_world"] $
    sexp [text "store", mlfVar (UN "ref"), show 0, mlfVar (UN "x")]
mlfExtPrim (NS _ (UN "prim__schemeCall")) =
  mlfLam [UN "_rTy", UN "fn", UN "_args", UN "_world"] $
    mlfLibCall "Stdlib.failwith" [mlfVar (UN "fn")]
mlfExtPrim (NS _ (UN "prim__codegen")) = mlfString "malfunction"
mlfExtPrim (NS _ (UN "prim__os")) = mlfGlobal "Rts.System.os_name"
mlfExtPrim n = mlfError $ "unimplemented external primitive: " ++ show n

mlfConstant : Constant -> Doc
mlfConstant (I x) = show x
mlfConstant (BI x) = show x <+> text ".ibig"
mlfConstant (Str s) = mlfString s
mlfConstant (Ch x) = show (ord x)
mlfConstant (Db x) =
  case filter (== '.') (unpack $ Prelude.show x) of
    [] => Pretty.show x <+> text ".0"
    _  => Pretty.show x

mlfConstant (B8 x) = mlfError $ "B8: " ++ show x
mlfConstant (B16 x) = mlfError $ "B16: " ++ show x
mlfConstant (B32 x) = mlfError $ "B32: " ++ show x
mlfConstant (B64 x) = mlfError $ "B64: " ++ show x

mlfConstant WorldVal = show 0

mlfConstant IntType = show 0
mlfConstant IntegerType = show 1
mlfConstant StringType = show 2
mlfConstant CharType = show 3
mlfConstant DoubleType = show 4
mlfConstant WorldType = show 5
mlfConstant Bits8Type = show 6
mlfConstant Bits16Type = show 7
mlfConstant Bits32Type = show 8
mlfConstant Bits64Type = show 9

mlfConstPat : Constant -> Maybe Doc
-- malfunction cannot switch on these
mlfConstPat (BI x) = Nothing
mlfConstPat (Str s) = Nothing
mlfConstPat (Db x) = Nothing
mlfConstPat c = Just $ mlfConstant c

mlfConstEqCheck : Doc -> Constant -> Doc
-- these have special comparison ops
mlfConstEqCheck x (BI y) = sexp [text "==.ibig", x, mlfConstant (BI y)]
mlfConstEqCheck x (Db y) = sexp [text "==.f64", x, mlfConstant (Db y)]
mlfConstEqCheck x (Str y) = mlfLibCall "String.equal" [x, mlfConstant (Str y)]

-- everything else is represented as ints
mlfConstEqCheck x y = sexp [text "==.int", x, mlfConstant y]

mlfConDflt : Doc -> Doc
mlfConDflt rhs = sexp [sexp [text "tag", text "_"], text "_", rhs]

mlfSwitch : Doc -> List Doc -> Maybe Doc -> Doc
mlfSwitch scrut [] Nothing =
  mlfError $ "case with no RHS"
mlfSwitch scrut [] (Just dflt) = dflt
mlfSwitch scrut alts (Just dflt) = parens $
  text "switch" <++> scrut
  $$ indent (vcat alts $$ dflt)

mlfSwitch scrut alts Nothing = parens $
  text "switch" <++> scrut
  $$ indent (if debug then (vcat alts $$ catchall) else vcat alts)
 where
  catchall : Doc
  catchall =
    mlfConDflt $
      mlfLet (UN "_") (mlfLibCall "Rts.Debug.inspect" [show 0, scrut])$
        mlfError "unmatched pattern! (block tree dump above)"

mlfConstDflt : Doc -> Doc
mlfConstDflt rhs = sexp [text "_", rhs]

mlfField : Name -> Int -> Doc
mlfField n i = sexp [text "field", show i, mlfVar n]

number : Int -> List a -> List (Int, a)
number i [] = []
number i (x :: xs) = (i,x) :: number (i+1) xs

bindFieldProjs : Name -> List Name -> Doc -> Doc
bindFieldProjs scrutN [] rhs = rhs
bindFieldProjs scrutN ns rhs = parens $
  text "let"
  $$ indent (
    vcat [sexp [mlfVar n, mlfField scrutN i] | (i, n) <- number 0 ns]
    $$ rhs
  )

ccLibFun : List String -> Maybe String
ccLibFun [] = Nothing
ccLibFun (cc :: ccs) =
  if substr 0 3 cc == "ML:"
    then Just (substr 3 (length cc) cc)
    else if substr 0 2 cc == "C:"
        then case split (== ',') (substr 2 (length cc) cc) of
          [fn, libn] => Just ("Rts.C.Lib_" ++ trim libn ++ "." ++ fn)
          _ => ccLibFun ccs  -- something strange -> skip
        else ccLibFun ccs  -- search further

{-
unApp : NamedCExp -> List NamedCExp -> (NamedCExp, List NamedCExp)
unApp (NmApp fc f args) args' = unApp f (args ++ args')
unApp f args = (f, args)
-}

parameters (ldefs : SortedSet Name)
  mutual
    bindScrut : NamedCExp -> (Name -> Doc) -> Doc
    bindScrut (NmLocal _ n) rhs = rhs n
    bindScrut scrut rhs =
      let scrutN = MN "scrut" 0
        in mlfLet scrutN (mlfTm scrut) (rhs scrutN)

    mlfEqChain : Name -> Maybe Doc -> List NamedConstAlt -> Doc
    mlfEqChain scrutN Nothing [] = mlfError "impossible eq chain"
    mlfEqChain scrutN (Just dflt) [] = dflt
    mlfEqChain scrutN mbDflt (MkNConstAlt c rhs :: alts) = parens $
      text "if" <++> mlfConstEqCheck (mlfVar scrutN) c
      $$ indent (
        mlfTm rhs
        $$ mlfEqChain scrutN mbDflt alts
      )

    mlfConAlt : Name -> NamedConAlt -> Doc
    mlfConAlt scrutN (MkNConAlt n Nothing args rhs) =
      mlfError $ "no tag for mlfConAlt: " ++ show n
    mlfConAlt scrutN (MkNConAlt cn (Just tag) [] rhs) =
      -- nullary constructors compile to ints in ocaml
      sexp [show tag, mlfTm rhs]
    mlfConAlt scrutN (MkNConAlt cn (Just tag) args rhs) = parens $
      sexp [text "tag", show tag]
      $$ indent (bindFieldProjs scrutN args $ mlfTm rhs)

    mlfConstAlt : NamedConstAlt -> Maybe Doc
    mlfConstAlt (MkNConstAlt c rhs) =
      case mlfConstPat c of
        Just pat => Just $ parens (pat <++> mlfTm rhs)
        Nothing => Nothing

    mlfTm : NamedCExp -> Doc
    mlfTm (NmLocal fc n) = mlfVar n
    mlfTm (NmRef fc n) =
      if contains n ldefs
        then mlfForce (mlfVar n)
        else mlfVar n
    mlfTm (NmLam fc n rhs) = mlfLam [n] (mlfTm rhs)
    mlfTm (NmLet fc n val rhs) = mlfLet n (mlfTm val) (mlfTm rhs)
    mlfTm (NmApp fc f args) =
      mlfApply (mlfTm f) (map mlfTm args)
      {- probably unnecessary
      let (f', args') = unApp f args
        in mlfApply (mlfTm f') (map mlfTm args')
      -}
    mlfTm (NmCon fc cn Nothing []) = mlfString (show cn)  -- type constructor
    mlfTm (NmCon fc cn (Just tag) []) = show tag
    mlfTm (NmCon fc cn mbTag args) = mlfBlock mbTag (map mlfTm args)
    mlfTm (NmCrash fc msg) = mlfError msg
    mlfTm (NmForce fc rhs) = mlfForce (mlfTm rhs)
    mlfTm (NmDelay fc rhs) = mlfLazy (mlfTm rhs)
    mlfTm (NmErased fc) = mlfString "erased"
    mlfTm (NmPrimVal ft x) = mlfConstant x
    mlfTm (NmOp fc op args) = mlfOp op (map mlfTm args)
    mlfTm (NmExtPrim fc n args) = mlfApply (mlfExtPrim n) (map mlfTm args)
    mlfTm (NmConCase fc scrut alts mbDflt) =
      bindScrut scrut $ \scrutN =>
        mlfSwitch
          (mlfVar scrutN)
          (map (mlfConAlt scrutN) alts)
          (mlfConDflt . mlfTm <$> mbDflt)
    mlfTm (NmConstCase fc scrut alts mbDflt) =
      case the (Maybe (List Doc)) (traverse mlfConstAlt alts) of
        -- all patterns can be expressed efficiently
        Just alts' =>
          mlfSwitch (mlfTm scrut) alts' (mlfConstDflt . mlfTm <$> mbDflt)

        -- we need to use a chain of if-equals tests
        Nothing =>
          bindScrut scrut $ \scrutN =>
            mlfEqChain scrutN (mlfTm <$> mbDflt) alts

  mlfBody : NamedDef -> Doc
  mlfBody (MkNmFun args rhs) =
    mlfLam args (mlfTm rhs)

  mlfBody (MkNmCon mbTag arity mbNewtype) =
      mlfLam args (mlfBlock mbTag $ map mlfVar args)
    where
      args : List Name
      args = [UN $ "arg" ++ show i | i <- [0..cast {to = Int} arity-1]]

  mlfBody (MkNmForeign ccs fargs cty) =
    mlfLam (map fst args) $
      case ccLibFun ccs of
        Just fn => mlfLibCall fn (map mlfVar fgnArgs)
        Nothing =>
          mlfError $ "unimplemented foreign: " ++ show (MkNmForeign ccs fargs cty)
    where
      mkArgs : Int -> List CFType -> List (Name, Bool)
      mkArgs i [] = []
      mkArgs i (CFWorld :: cs) = (MN "farg" i, False) :: mkArgs i cs
      mkArgs i (c :: cs) = (MN "farg" i, True) :: mkArgs (i + 1) cs

      args : List (Name, Bool)
      args = mkArgs 0 fargs

      fgnArgs : List Name
      fgnArgs = map fst $ filter snd args

  mlfBody (MkNmError err) =
    mlfTm err

  mlfDef : (Name, FC, NamedDef) -> Doc
  mlfDef (n, fc, body) =
    parens (mlfVar n $$ indent (mlfBody body))
    $$ text ""

lazyDefs : List (Name, FC, NamedDef) -> SortedSet Name
lazyDefs [] = empty
lazyDefs ((n,_,MkNmFun [] rhs) :: defs) = insert n $ lazyDefs defs
lazyDefs ((n,_,MkNmCon tag Z nt) :: defs) = insert n $ lazyDefs defs
lazyDefs ((n,_,MkNmForeign ccs [] x) :: defs) = insert n $ lazyDefs defs
lazyDefs ((n,_,MkNmError x) :: defs) = insert n $ lazyDefs defs
lazyDefs (_ :: defs) = lazyDefs defs

mlfRec : List Doc -> Doc
mlfRec defs = parens $
  text "rec"
  $$ indentBlock defs

generateMlf : Ref Ctxt Defs ->
               ClosedTerm -> (outfile : String) -> Core ()
generateMlf c tm outfile = do
  cdata <- getCompileData Cases tm
  let ndefs = namedDefs cdata
  let ctm = forget (mainExpr cdata)
  let ldefs = lazyDefs ndefs

  defsMlf <- traverse (pure . mlfDef ldefs) ndefs
  mainMlf <- pure $ mlfTm ldefs ctm
  let code = render " " $ parens (
        text "module"
        $$ indent (
             mlfRec defsMlf
          $$ parens (text "_" <++> mainMlf)
          $$ text ""
          $$ parens (text "export")
          )
        )
        $$ text ""
        $$ text "; vim: ft=lisp"
        $$ text ""  -- end with a newline
  Right () <- coreLift $ writeFile outfile code
    | Left err => throw (FileErr outfile err)
  pure ()

compileExpr : Ref Ctxt Defs -> (execDir : String) ->
              ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c execDir tm outfile = do
  let bld = execDir </> "mlf-" ++ outfile
  coreLift $ mkdirAll bld

  let copy = \fn => with Core.Core.(>>=) do
       src <- readDataFile ("malfunction" </> fn)
       coreLift $ writeFile (bld </> fn) src

  -- TMP HACK
  coreLift $ system $ unwords
    ["cp", "~/.idris2/idris2-0.2.0/lib/libidris2_support.a", bld]

  copy "LowLevel.mlf"
  copy "LowLevel.mli"
  copy "Rts.ml"
  copy "rts.c"
  generateMlf c tm (bld </> "Main.mlf")

  let flags = if debug then "-g" else ""
  let cmd = unwords
        [ "(cd " ++ bld
        -- clean up
        , "&& { rm -f Rts.mli *.cmi *.cmx *.o || true; }"
        -- C
        , "&& cc -O2 " ++ flags ++ " -c rts.c -I $(ocamlc -where)"
        -- LowLevel
        , "&& ocamlfind opt -I +threads " ++ flags ++ " -c LowLevel.mli"
        , "&& malfunction cmx LowLevel.mlf"
        -- Rts
        , "&& ocamlfind opt -I +threads " ++ flags ++ " -i Rts.ml > Rts.mli"
        , "&& ocamlfind opt -I +threads " ++ flags ++ " -c Rts.mli"
        , "&& ocamlfind opt -I +threads " ++ flags ++ " -c Rts.ml"
        -- Main
        , "&& malfunction cmx Main.mlf"
        -- link it all together
        , "&& ocamlfind opt -thread -package zarith -linkpkg -nodynlink "
            ++ flags ++ " rts.o libidris2_support.a LowLevel.cmx Rts.cmx Main.cmx -o ../" ++ outfile
        , ")"
        ]
  ok <- coreLift $ system cmd
  if ok == 0
    then pure (Just (execDir </> outfile))
    else pure Nothing

executeExpr : Ref Ctxt Defs -> (execDir : String) -> ClosedTerm -> Core ()
executeExpr c execDir tm
    = do outn <- compileExpr c execDir tm "_tmp_mlf"
         case outn of
              -- TODO: on windows, should add exe extension
              Just outn => map (const ()) $ coreLift $ system outn
              Nothing => pure ()

export
codegenMalfunction : Codegen
codegenMalfunction = MkCG compileExpr executeExpr
