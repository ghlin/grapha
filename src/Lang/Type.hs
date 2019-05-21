module Lang.Type where

import           Data.List                      ( union )
import           Misc

data Scheme     = Forall [Name] Type deriving (Show, Eq)

data Type
  = TVar Name        -- ^ 类型变量
  | TCon Name        -- ^ 类型构造器(名)
  | TApp Type Type   -- ^ 应用
  deriving (Show, Eq)

tInt, tString, tBool, tUnit, tFun :: Type
tInt    = TCon "Int"
tString = TApp (TCon "[]") tChar
tChar   = TCon "Char"
tBool   = TCon "Bool"
tUnit   = TCon "()"
tFun    = TCon "->"

tTupleCon :: Int -> Type
tTupleCon n = TCon $ tupleCon n

fn :: Type -> Type -> Type
fn a b = TApp (TApp tFun a) b

tFVs :: Type -> [Name]
tFVs (TVar n  ) = [n]
tFVs (TApp l r) = tFVs l `union` tFVs r
tFVs _          = []
