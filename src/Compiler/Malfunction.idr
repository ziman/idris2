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
mlfApply f args = sexp (text "apply" :: f :: args)

mlfLibCall : String -> List Doc -> Doc
mlfLibCall fn args = mlfApply (mlfGlobal fn) args

mlfDebug : Show a => a -> Doc
mlfDebug x = mlfLibCall "Stdlib.failwith" [mlfString $ show x]

mlfName : Name -> Doc
mlfName = text . pack . sanitise . unpack . show
  where
    san : Char -> Bool
    san c =
        ('A' <= c && c <= 'Z')
        || ('a' <= c && c <= 'z')
        || ('0' <= c && c <= '9')

    sanitise : List Char -> List Char
    sanitise [] = []
    sanitise (c :: cs) =
      case (san c, sanitise cs) of
        (True,  [])         => [c]
        (False, [])         => []
        (True,   k  :: cs') =>  c  ::  k  :: cs'
        (False, '-' :: cs') =>        '-' :: cs'
        (False,  k  :: cs') => '-' ::  k  :: cs'

mlfVar : Name -> Doc
mlfVar n = text "$" <+> mlfName n

{-
     -- Normal function definition
     MkNmFun : (args : List Name) -> NamedCExp -> NamedDef
     -- Constructor
     MkNmCon : (tag : Maybe Int) -> (arity : Nat) -> (nt : Maybe Nat) -> NamedDef
     -- Foreign definition
     MkNmForeign : (ccs : List String) ->
                   (fargs : List CFType) ->
                   CFType ->
                   NamedDef
     -- A function which will fail at runtime (usually due to being a hole) so needs
     -- to run, discarding arguments, no matter how many arguments are passed
     MkNmError : NamedCExp -> NamedDef
-}

mlfTm : NamedCExp -> Core Doc
mlfTm tm = pure $ mlfDebug tm

mlfLazy : Doc -> Doc
mlfLazy doc = sexp [text "lazy", doc]

mlfForce : Doc -> Doc
mlfForce doc = sexp [text "force", doc]

mlfLam : List Name -> Doc -> Doc
mlfLam args rhs = sexp [text "lambda", sexp $ map mlfVar args, rhs]

mlfBody : NamedDef -> Core Doc
mlfBody (MkNmFun args rhs) =
  mlfLam args <$> mlfTm rhs

mlfBody (MkNmCon mbTag arity mbNewtype) =
  pure $ mlfLazy $ mlfDebug (MkNmCon mbTag arity mbNewtype)

mlfBody (MkNmForeign ccs fargs cty) =
  pure $ mlfLazy $ mlfDebug (MkNmForeign ccs fargs cty)

mlfBody (MkNmError err) =
  mlfTm err

mlfDef : (Name, FC, NamedDef) -> Core Doc
mlfDef (n, fc, body) = do
  body' <- mlfBody body
  pure $ parens (mlfVar n $$ indent body')

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
         let code = render "  " $ parens (
                text "module"
                $$ indent (
                     vcat defsMlf
                  $$ parens (text "_" <++> mainMlf)
                  $$ parens (text "export")
                )
               ) $$ text ""  -- end with a newline
         Right () <- coreLift $ writeFile outfile code
            | Left err => throw (FileErr outfile err)
         pure ()

compileExpr : Ref Ctxt Defs -> (execDir : String) ->
              ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c execDir tm outfile
    = do let mlf = execDir </> outfile <.> "mlf"
             exe = execDir </> outfile
         compileToMLF c tm mlf
         let cmd = "malfunction compile -o " ++ exe ++ " " ++ mlf
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
