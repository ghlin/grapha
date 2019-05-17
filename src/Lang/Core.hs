module Lang.Core where

import           Control.Monad                  ( foldM )
import           Misc                           ( Name
                                                , tupleCon
                                                )
import qualified Lang.Surface                  as S

data Kind
  = KStar
  | KFun  Kind Kind
  deriving (Show, Eq)

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

data Pat
  = PLit Ty S.Literal
  | PVar Ty Name
  | PWildcard
  | PCon Ty [Pat]
  deriving (Show, Eq)

data Expr
  = ELit  Ty  S.Literal
  | EVar  Sc  Name
  | ELam  Ty  [Alt] Expr
  | EApp      Expr  Expr
  | EIf       Expr  Expr Expr
  | ELet      [Alt] Expr
  | ECase     Expr  [Alt]
  deriving (Show, Eq)

data Alt
  = Alt Name Sc Expr
  deriving (Show, Eq)

fromArity :: Int -> Kind
fromArity 0 = KStar
fromArity n = KFun KStar $ fromArity $ n - 1

applyK :: Monad m => Kind -> Kind -> m Kind
applyK (KFun k1 k2) k | k1 == k = return k2
applyK _            _           = fail "Kind mismatch"

applyKs :: Monad m => Kind -> [Kind] -> m Kind
applyKs = foldM applyK


tInt, tString, tUnit :: Ty
tInt    = TCon $ TyCon "Int" KStar
tString = TCon $ TyCon "String" KStar
tUnit   = TCon $ TyCon "()" KStar
tFun    = TCon $ TyCon "()" $ fromArity 2

tTupleCon :: Int -> Ty
tTupleCon n = TCon $ TyCon (tupleCon n) $ fromArity n

fn :: Ty -> Ty -> Ty
fn a b = TApp (TApp tFun a) b


