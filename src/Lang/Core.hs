module Lang.Core where

import           Misc                           ( Name )
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

