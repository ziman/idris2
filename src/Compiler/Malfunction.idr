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

import System
import System.Directory
import System.File
import System.Info

%default covering

showChar : Char -> String -> String
showChar '\\' = ("\\\\" ++)
showChar c
   = if c < chr 32 -- XXX
        then (("\\x" ++ asHex (cast c) ++ ";") ++)
        else strCons c

showString : List Char -> String -> String
showString [] = id
showString ('"'::cs) = ("\\\"" ++) . showString cs
showString (c::cs) = showChar c . showString cs

mlfString : String -> String
mlfString cs = strCons '"' (showString (unpack cs) "\"")

mlfDef : (Name, (FC, NamedDef)) -> Core Doc
mlfDef def = ?rhsDef

mlfTm : NamedCExp -> Core Doc
mlfTm tm = ?rhsTm

compileToMLF : Ref Ctxt Defs ->
               ClosedTerm -> (outfile : String) -> Core ()
compileToMLF c tm outfile
    = do cdata <- getCompileData Cases tm
         let ndefs = namedDefs cdata
         -- let tags = nameTags cdata
         let ctm = forget (mainExpr cdata)

         {-
         defs <- get Ctxt
         l <- newRef {t = List String} Loaded []
         s <- newRef {t = List String} Structs []
         -}

         defsMlf <- traverse mlfDef ndefs
         mainMlf <- mlfTm ctm
         let code = render "  " $ parens $
                text "module"
                $$ indent (vcat defsMlf $$ mainMlf $$ parens (text "export"))
         Right () <- coreLift $ writeFile outfile code
            | Left err => throw (FileErr outfile err)
         pure ()

compileExpr : Ref Ctxt Defs -> (execDir : String) ->
              ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c execDir tm outfile
    = do let mlf = execDir </> outfile <.> "mlf"
             exe = execDir </> outfile
         compileToMLF c tm mlf
         let cmd = "malfunction -o " ++ exe ++ " " ++ mlf
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
