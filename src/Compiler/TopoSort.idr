module Compiler.TopoSort

import public Core.FC
import public Core.Name
import public Compiler.CompileExpr

%default total

export
topoSort : List (Name, FC, NamedDef) -> List (List (Name, FC, NamedDef))
topoSort defs = ?rhs
