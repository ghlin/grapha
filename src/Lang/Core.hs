module Lang.Core where

import           Lang.Type
import           Lang.Literal
import           Misc

data CoreCombinator
  = CoreCombinator Name [Name] CoreExpr
  deriving (Show, Eq)

data CoreExpr
  = ELit  Literal
  | EVar  Name
  | ELam  [Name]   CoreExpr
  | EApp  CoreExpr CoreExpr
  | EIf   CoreExpr CoreExpr CoreExpr
  | ELet  [CoreCombinator]  CoreExpr
  | ETest Name     CoreExpr  -- ^ only the value constructor
  | EPick Name Int CoreExpr
  deriving (Show, Eq)

