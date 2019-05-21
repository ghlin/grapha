module Lang.SC where

import           Lang.Literal
import           Misc

data SC
  = SC Name [Name] SCExpr
  deriving (Show)

data SCExpr
  = SCLit     Literal
  | SCVar     Name
  | SCApp     SCExpr SCExpr
  | SCBuiltin Name Int -- ^ name & arity
  | SCPack    Name Int -- ^ constructor & arity
  | SCPick    Int      -- ^ which component to pick
  | SCTest    Name     -- ^ test if of a given constructor
  | SCIf      SCExpr SCExpr SCExpr
  | SCLet     Name SCExpr SCExpr
  | SCLetRec  [(Name, SCExpr)] SCExpr -- ^ mutal rec let
  deriving (Show)

type SCProgram = ([SC], Name)    -- ^ sequence of sc defns, and the entry sc name

