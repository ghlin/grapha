module Lang.Simpl
where

import           Lang.Type
import           Lang.Kind
import           Lang.Literal
import           Misc

data SimplDataTypeDef
  = SimplDataTypeDef TyCon [Kind] [(Name, SimplProductDef)]
  deriving (Show, Eq)

data SimplProductDef
  = SimplProductDef TyCon [Ty]
  deriving (Show, Eq)

data SimplTypeClassDef
  = SimplTypeClassDef TyCon [Pred] [(Name, Sc)]
  deriving (Show, Eq)

data SimplInstanceDef
  = SimplInstanceDef Name Ty [Pred] [SimplCombinatorDef]
  deriving (Show, Eq)

data SimplCombinatorDef
  = SimplCombinatorDef Name [Name] SimplExpr
  deriving (Show, Eq)

data SimplExpr
  = ELit  Literal
  | EVar  Name
  | ELam  [Name]     SimplExpr
  | EApp  SimplExpr  SimplExpr
  | EIf   SimplExpr  SimplExpr  SimplExpr
  | ELet  [SimplCombinatorDef]  SimplExpr
  | ETest Name SimplExpr  -- ^ only the value constructor
  | EPick Int  SimplExpr
  deriving (Show, Eq)

