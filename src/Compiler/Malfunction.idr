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

mlfOp : PrimFn arity -> Vect arity Doc -> Doc
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
mlfExtPrim n = mlfDebug n

mlfConstant : Constant -> Doc
mlfConstant (I x) = show x <+> text ".i64"
mlfConstant (BI x) = show x <+> text ".ibig"
mlfConstant (Str s) = mlfString s
mlfConstant (Ch x) = show (ord x)
mlfConstant (Db x) = show x
mlfConstant WorldVal = mlfString "%World"
mlfConstant IntType = mlfString "TyInt"
mlfConstant IntegerType = mlfString "TyInteger"
mlfConstant StringType = mlfString "TyString"
mlfConstant CharType = mlfString "TyChar"
mlfConstant DoubleType = mlfString "TyDouble"
mlfConstant WorldType = mlfString "TyWorld"

mlfSwitch : Doc -> List Doc -> Maybe Doc -> Doc
mlfSwitch scrut alts (Just dflt) = parens $
  text "switch" <++> scrut
  $$ indentBlock alts
  $$ sexp [text "_", sexp [text "tag", text "_"], dflt]

mlfSwitch scrut alts Nothing = parens $
  text "switch" <++> scrut
  $$ indentBlock alts

mutual
  mlfConAlt : NamedConAlt -> Doc
  mlfConAlt (MkNConAlt n Nothing args rhs) =
    mlfError $ "no tag for mlfConAlt: " ++ show n
  mlfConAlt (MkNConAlt n (Just tag) args rhs) =
    sexp [sexp [text "tag", show tag], mlfTm rhs]

  mlfConstAlt : NamedConstAlt -> Doc
  mlfConstAlt (MkNConstAlt c rhs) =
    parens (mlfConstant c <++> mlfTm rhs)

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
  mlfTm (NmPrimVal ft x) = mlfConstant x
  mlfTm (NmOp fc op args) = mlfOp op (map mlfTm args)
  mlfTm (NmExtPrim fc n args) = mlfApply (mlfExtPrim n) (map mlfTm args)
  mlfTm (NmConCase fc scrut alts mbDflt) =
    mlfSwitch (mlfTm scrut) (map mlfConAlt alts) (mlfTm <$> mbDflt)
  mlfTm (NmConstCase fc scrut alts mbDflt) =
    mlfSwitch (mlfTm scrut) (map mlfConstAlt alts) (mlfTm <$> mbDflt)

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