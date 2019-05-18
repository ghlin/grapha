module Lang.Type
where

import           Lang.Kind
import           Misc

data TyVar = TyVar Name Kind deriving (Show, Eq)
data TyCon = TyCon Name Kind deriving (Show, Eq)

data Pred   = Pred Name   Ty      deriving (Show, Eq)
data Qual t = Qual [Pred] t       deriving (Show, Eq)
data Sc     = Sc [Kind] (Qual Ty) deriving (Show, Eq)

data Ty
  = TVar TyVar
  | TCon TyCon
  | TApp Ty Ty
  | TGen Int
  deriving (Show, Eq)

tInt, tString, tUnit :: Ty
tInt    = TCon $ TyCon "Int" KStar
tString = TCon $ TyCon "String" KStar
tUnit   = TCon $ TyCon "()" KStar
tBool   = TCon $ TyCon "Bool" KStar
tFun    = TCon $ TyCon "()" $ fromArity 2

tTupleCon :: Int -> Ty
tTupleCon n = TCon $ TyCon (tupleCon n) $ fromArity n

fn :: Ty -> Ty -> Ty
fn a b = TApp (TApp tFun a) b
