module Lang.Kind where

import           Control.Monad                  ( foldM )

data Kind
  = KStar
  | KFun  Kind Kind
  deriving (Show, Eq)

fromArity :: Int -> Kind
fromArity 0 = KStar
fromArity n = KFun KStar $ fromArity $ n - 1

applyK :: Monad m => Kind -> Kind -> m Kind
applyK (KFun k1 k2) k | k1 == k = return k2
applyK _            _           = fail "Kind mismatch"

applyKs :: Monad m => Kind -> [Kind] -> m Kind
applyKs = foldM applyK




