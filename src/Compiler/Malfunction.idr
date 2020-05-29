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
mlfApply f args = parens $
  text "apply" <++> f
  $$ indentBlock args

mlfLibCall : String -> List Doc -> Doc
mlfLibCall fn args = mlfApply (mlfGlobal fn) args

mlfError : String -> Doc
mlfError msg = mlfLibCall "Stdlib.failwith" [mlfString msg]

mlfDebug : Show a => a -> Doc
mlfDebug = mlfError . show

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

mlfLet : Name -> Doc -> Doc -> Doc
mlfLet n val rhs = sexp [text "let", sexp [mlfVar n, val], rhs]

mlfLam : List Name -> Doc -> Doc
mlfLam args rhs =
  parens $
    text "lambda" <++> sexp (map mlfVar args)
    $$ indent rhs

mlfLazy : Doc -> Doc
mlfLazy doc = sexp [text "lazy", doc]

mlfForce : Doc -> Doc
mlfForce doc = sexp [text "force", doc]

mlfBlock : Maybe Int -> List Doc -> Doc
mlfBlock Nothing args = mlfError "no constructor tag"
mlfBlock (Just tag) args = parens $
  text "block" <++> sexp [text "tag", show tag]
  $$ indentBlock args

mlfTm : NamedCExp -> Doc
mlfTm (NmLocal fc n) = mlfVar n
mlfTm (NmRef fc n) = mlfVar n
mlfTm (NmLam fc n rhs) = mlfLam [n] (mlfTm rhs)
mlfTm (NmLet fc n val rhs) = mlfLet n (mlfTm val) (mlfTm rhs)
mlfTm (NmApp fc f args) = mlfApply (mlfTm f) (map mlfTm args)
mlfTm (NmCon fc cn mbTag args) = mlfBlock mbTag (map mlfTm args)
mlfTm (NmCrash fc msg) = mlfError msg
mlfTm (NmForce fc rhs) = mlfForce (mlfTm rhs)
mlfTm (NmDelay fc rhs) = mlfLazy (mlfTm rhs)
mlfTm (NmErased fc) = mlfString "erased"
mlfTm (NmPrimVal fc (I x)) = show x <+> text ".i64"
mlfTm (NmPrimVal fc (BI x)) = show x <+> text ".ibig"
mlfTm (NmPrimVal fc (Str s)) = mlfString s
mlfTm (NmPrimVal fc (Ch x)) = show (ord x)
mlfTm (NmPrimVal fc (Db x)) = show x
mlfTm (NmPrimVal fc WorldVal) = mlfString "world"
mlfTm (NmPrimVal fc IntType) = mlfString "tyInt"
mlfTm (NmPrimVal fc IntegerType) = mlfString "tyInteger"
mlfTm (NmPrimVal fc StringType) = mlfString "tyString"
mlfTm (NmPrimVal fc CharType) = mlfString "tyChar"
mlfTm (NmPrimVal fc DoubleType) = mlfString "tyDouble"
mlfTm (NmPrimVal fc WorldType) = mlfString "tyWorld"
mlfTm tm = mlfDebug tm

{-
mlfTm (NmOp fc x xs) = ?rhs_7
mlfTm (NmExtPrim fc p xs) = ?rhs_8
mlfTm (NmConCase fc sc xs x) = ?rhs_11
mlfTm (NmConstCase fc sc xs x) = ?rhs_12
mlfTm (NmPrimVal fc x) = ?rhs_13
-}

mlfBody : NamedDef -> Doc
mlfBody (MkNmFun args rhs) =
  mlfLam args (mlfTm rhs)

mlfBody (MkNmCon mbTag arity mbNewtype) =
  mlfLazy $ mlfDebug (MkNmCon mbTag arity mbNewtype)

mlfBody (MkNmForeign ccs fargs cty) =
  mlfLazy $ mlfDebug (MkNmForeign ccs fargs cty)

mlfBody (MkNmError err) =
  mlfTm err

mlfDef : (Name, FC, NamedDef) -> Doc
mlfDef (n, fc, body) =
  parens (mlfVar n $$ indent (mlfBody body))
  $$ text ""

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

         defsMlf <- traverse (pure . mlfDef) ndefs
         mainMlf <- pure $ mlfTm ctm
         let code = render " " $ parens (
                text "module"
                $$ indent (
                     vcat defsMlf
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
