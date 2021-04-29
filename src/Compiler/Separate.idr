module Compiler.Separate

import public Core.FC
import public Core.Name
import public Core.Name.Namespace
import public Core.CompileExpr
import public Libraries.Data.SortedMap
import public Libraries.Data.SortedSet
import public Libraries.Data.StringMap

import Core.Hash
import Data.List
import Data.Vect
import Data.Maybe

export
record CompilationUnitId where
  constructor CUID
  int : Int

export
Eq CompilationUnitId where
  CUID x == CUID y = x == y

export
Ord CompilationUnitId where
  compare (CUID x) (CUID y) = compare x y

export
Hashable CompilationUnitId where
  hashWithSalt h (CUID int) = hashWithSalt h int

-- A compilation unit is a set of namespaces.
public export
record CompilationUnit def where
  constructor MkCompilationUnit
  id : CompilationUnitId
  namespaces : List String
  dependencies : SortedSet CompilationUnitId
  definitions : List (Name, def)

export
Hashable def => Hashable (CompilationUnit def) where
  hashWithSalt h cu =
    h `hashWithSalt` SortedSet.toList cu.dependencies
      `hashWithSalt` cu.definitions

private
getNS : Name -> String
getNS (NS ns _) = showNSWithSep "-" ns
getNS _ = ""

namespace StringSet
  public export  -- otherwise Idris can't find the Monoid/Semigroup instances
  StringSet : Type
  StringSet = StringMap ()

  export
  fromList : List String -> StringSet
  fromList xs = StringMap.fromList [(x, ()) | x <- xs]

  export
  toList : StringSet -> List String
  toList = StringMap.keys

  export
  union : StringSet -> StringSet -> StringSet
  union = StringMap.mergeLeft

  export
  empty : StringSet
  empty = StringMap.empty

  export
  singleton : String -> StringSet
  singleton x = StringMap.singleton x ()

  export
  delete : String -> StringSet -> StringSet
  delete = StringMap.delete

-- https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm#The_algorithm_in_pseudocode
private
record TarjanVertex where
  constructor TV
  index : Int
  lowlink : Int
  inStack : Bool

private
record TarjanState where
  constructor TS
  vertices : StringMap TarjanVertex
  stack : List String
  nextIndex : Int
  components : List (List String)
  impossibleHappened : Bool

private
tarjan : StringMap StringSet -> List (List String)
tarjan deps = loop initialState (StringMap.keys deps)
  where
    initialState : TarjanState
    initialState =
      TS
        StringMap.empty
        []
        0
        []
        False

    strongConnect : TarjanState -> String -> TarjanState
    strongConnect ts v =
        let ts'' = case StringMap.lookup v deps of
              Nothing => ts'  -- no edges
              Just edgeSet => loop ts' (StringSet.toList edgeSet)
          in case StringMap.lookup v ts''.vertices of
              Nothing => record { impossibleHappened = True } ts''
              Just vtv =>
                if vtv.index == vtv.lowlink
                  then createComponent ts'' v []
                  else ts''
      where
        createComponent : TarjanState -> String -> List String -> TarjanState
        createComponent ts v acc =
          case ts.stack of
            [] => record { impossibleHappened = True } ts
            w :: ws =>
              let ts' : TarjanState = record {
                      vertices $= StringMap.adjust w record{ inStack = False },
                      stack = ws
                    } ts
                in if w == v
                  then record { components $= ((v :: acc) ::) } ts'  -- that's it
                  else createComponent ts' v (w :: acc)

        loop : TarjanState -> List String -> TarjanState
        loop ts [] = ts
        loop ts (w :: ws) =
          loop (
            case StringMap.lookup w ts.vertices of
              Nothing => let ts' = strongConnect ts w in
                case StringMap.lookup w ts'.vertices of
                  Nothing => record { impossibleHappened = True } ts'
                  Just wtv => record { vertices $= StringMap.adjust v record{ lowlink $= min wtv.lowlink } } ts'

              Just wtv => case wtv.inStack of
                False => ts  -- nothing to do
                True => record { vertices $= StringMap.adjust v record{ lowlink $= min wtv.index } } ts
          ) ws

        ts' : TarjanState
        ts' = record {
            vertices  $= StringMap.insert v (TV ts.nextIndex ts.nextIndex True),
            stack     $= (v ::),
            nextIndex $= (1+)
          } ts

    loop : TarjanState -> List String -> List (List String)
    loop ts [] =
      if ts.impossibleHappened
        then []
        else ts.components
    loop ts (v :: vs) =
      case StringMap.lookup v ts.vertices of
        Just _ => loop ts vs  -- done, skip
        Nothing => loop (strongConnect ts v) vs

public export
interface HasNamespaces a where
  -- namespaces referred to from within
  nsRefs : a -> StringSet

mutual
  export
  HasNamespaces NamedCExp where
    nsRefs (NmLocal fc n) = StringSet.empty
    nsRefs (NmRef fc n) = StringSet.singleton $ getNS n
    nsRefs (NmLam fc n rhs) = nsRefs rhs
    nsRefs (NmLet fc n val rhs) = nsRefs val `StringSet.union` nsRefs rhs
    nsRefs (NmApp fc f args) = nsRefs f `StringSet.union` concatMap nsRefs args
    nsRefs (NmCon fc cn tag args) = concatMap nsRefs args
    nsRefs (NmForce fc reason rhs) = nsRefs rhs
    nsRefs (NmDelay fc reason rhs) = nsRefs rhs
    nsRefs (NmErased fc) = StringSet.empty
    nsRefs (NmPrimVal ft x) = StringSet.empty
    nsRefs (NmOp fc op args) = concatMap nsRefs args
    nsRefs (NmExtPrim fc n args) = concatMap nsRefs args
    nsRefs (NmConCase fc scrut alts mbDflt) =
      nsRefs scrut `StringSet.union` (concatMap nsRefs alts `StringSet.union` concatMap nsRefs mbDflt)
    nsRefs (NmConstCase fc scrut alts mbDflt) =
      nsRefs scrut `StringSet.union` (concatMap nsRefs alts `StringSet.union` concatMap nsRefs mbDflt)
    nsRefs (NmCrash fc msg) = StringSet.empty

  export
  HasNamespaces NamedConAlt where
    nsRefs (MkNConAlt n tag args rhs) = nsRefs rhs

  export
  HasNamespaces NamedConstAlt where
    nsRefs (MkNConstAlt c rhs) = nsRefs rhs

  export
  HasNamespaces NamedDef where
    nsRefs (MkNmFun argNs rhs) = nsRefs rhs
    nsRefs (MkNmCon tag arity nt) = StringSet.empty
    nsRefs (MkNmForeign ccs fargs rty) = StringSet.empty
    nsRefs (MkNmError rhs) = nsRefs rhs

-- a slight hack for convenient use with CompileData.namedDefs
export
HasNamespaces a => HasNamespaces (FC, a) where
  nsRefs (_, x) = nsRefs x

-- another slight hack for convenient use with CompileData.namedDefs
export
Hashable def => Hashable (FC, def) where
  -- ignore FC in hash
  hashWithSalt h (fc, x) = hashWithSalt h x

public export
record CompilationUnitInfo def where
  constructor MkCompilationUnitInfo
  compilationUnits : List (CompilationUnit def)  -- ordered by the number of imports, ascending
  byId : SortedMap CompilationUnitId (CompilationUnit def)

export
getCompilationUnits : HasNamespaces def => List (Name, def) -> CompilationUnitInfo def
getCompilationUnits {def} defs =
  MkCompilationUnitInfo
    units
    (SortedMap.fromList [(unit.id, unit) | unit <- units])
  where
    defsByNS : StringMap (List (Name, def))
    defsByNS = foldl addOne StringMap.empty defs
      where
        addOne
          : StringMap (List (Name, def))
          -> (Name, def)
          -> StringMap (List (Name, def))
        addOne nss ndef@(n, _) =
          StringMap.mergeWith
            List.(++)
            (StringMap.singleton (getNS n) [ndef])
            nss

    nsDepsRaw : List (StringMap StringSet)
    nsDepsRaw = [StringMap.singleton (getNS n) (StringSet.delete (getNS n) (nsRefs d)) | (n, d) <- defs]

    nsDeps : StringMap StringSet
    nsDeps = foldl (StringMap.mergeWith StringSet.union) StringMap.empty nsDepsRaw

    -- strongly connected components of the NS dep graph
    -- each SCC will become a compilation unit
    components : List (List String)
    components = List.reverse $ tarjan nsDeps  -- tarjan generates reverse toposort

    withCUID : List a -> List (CompilationUnitId, a)
    withCUID xs = [(CUID $ cast i, x) | (i, x) <- zip [0..length xs] xs]

    nsMap : StringMap CompilationUnitId
    nsMap = StringMap.fromList
      [(ns, cuid) | (cuid, nss) <- withCUID components, ns <- nss]

    mkUnit : CompilationUnitId -> List String -> CompilationUnit def
    mkUnit cuid nss = MkCompilationUnit cuid nss dependencies definitions
     where
      dependencies : SortedSet CompilationUnitId
      dependencies = SortedSet.fromList $ do
        ns <- nss  -- NS contained within
        depsNS <- StringSet.toList $  -- NS we depend on
          fromMaybe StringSet.empty $
            StringMap.lookup ns nsDeps

        case StringMap.lookup depsNS nsMap of
          Nothing => []
          Just depCUID => [depCUID]

      definitions : List (Name, def)
      definitions = concat
        [fromMaybe [] $ StringMap.lookup ns defsByNS | ns <- nss]


    units : List (CompilationUnit def)
    units = [mkUnit cuid nss | (cuid, nss) <- withCUID components]
