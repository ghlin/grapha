{-# LANGUAGE TupleSections #-}
module Lang.TypingEnv where

import           Control.Monad.Trans.State     as T
import           Control.Monad.Trans.Except    as E
import           Control.Monad.Trans.Writer    as W
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad                  ( foldM )
import           Data.Char                      ( isLetter )
import           Lang.Surface
import qualified Lang.Core                     as C
import           Lang.Typing             hiding ( ClassEnv(..)
                                                , unify
                                                )
import           Misc

predname :: Pred -> Name
predname (Pred n _) = n

classes   :: Program -> [(Name, [Name])]
classes = fmap pick . interfaceDefs
  where pick (InterfaceDef name preds _ _) = (name, predname <$> preds)

type TE a = Either String a
type KindSubst = [(Name, C.Kind)]

lookupKind :: Name -> KindSubst -> TE C.Kind
lookupKind i ks = case lookup i ks of
                    Just k  -> return k
                    Nothing -> Left $ "Unbound: " <> i

extendKS :: Name -> C.Kind -> KindSubst -> TE KindSubst
extendKS n k ks = case lookup n ks of
                    Nothing -> return $ (n, k):ks
                    Just k' -> if k == k' then return ks else Left $ "Kind mismatch: " <> n

-- | 断言:Type具有Kind: KStar
fromType :: KindSubst -> KindSubst -> Type -> TE KindSubst
fromType cks vks t = fromTypes cks vks C.KStar $ flatten t

-- | [Type]由某个Type经过flatten得到,这里断言那个Type具有给定的Kind
-- 在这个假设之下,推断Type内部符号的Kind
fromTypes :: KindSubst -> KindSubst -> C.Kind -> [Type] -> TE KindSubst
fromTypes cks vks = syn vks
  where syn  vks k (TVar f:rs) = do let ks' = replicate (length rs) C.KStar
                                    vks' <- synthsis cks vks ks' rs
                                    let k' = foldr C.KFun k ks'
                                    extendKS f k' vks'
        syn  vks k (TCon c:rs) = do ck <- lookupKind c cks
                                    let ks' = flattenK' ck
                                    vks' <- synthsis cks vks ks' rs
                                    k' <- applyKs ck ks'
                                    if k /= k'
                                       then Left "Kind mismatch"
                                       else return vks'


-- | 合成ts 和 ks
synthsis :: KindSubst -> KindSubst -> [C.Kind] -> [Type] -> TE KindSubst
synthsis _ _ ks ts | length ks /= length ts = Left "Kind mismatch"
synthsis cks vks ks ts                      = s $ ks `zip` ts
  where s              = foldM syn vks
        syn vks (k, t) = fromTypes cks vks k $ flatten t

fromProd :: KindSubst -> [Name] -> ProductDef -> TE KindSubst
fromProd ks vs (ProductDef _ ts) = do vks <- foldM ft [] ts
                                      return $ filter (onlyVs . fst) vks
                                      where ft       = fromType ks
                                            onlyVs n = n `elem` vs

dataTypeKinds :: KindSubst -> DataTypeDef -> TE KindSubst
dataTypeKinds ks (DataTypeDef name as ps) = do as' <- mconcat <$> mapM (fromProd ks as) ps
                                               kas <- mapM (flip lookupKind as') as
                                               extendKS name (foldr C.KFun C.KStar kas) ks
data ClassDef = ClassDef Name [C.Pred] C.TyVar
  deriving (Show, Eq)

data InstDef = InstDef Name [C.Pred] C.Ty
  deriving (Show, Eq)

flatten :: Type -> [Type]
flatten (TApp l r) = flatten l <> [r]
flatten v          = [v]

flattenK :: C.Kind -> [C.Kind]
flattenK (C.KFun l r) = flattenK l <> [r]
flattenK k            = [k]

flattenK' :: C.Kind -> [C.Kind]
flattenK' = reverse . tail . reverse . flattenK

applyK :: C.Kind -> C.Kind -> TE C.Kind
applyK (C.KFun k1 k2) k | k1 == k = return k2
applyK _              _           = Left "Kind mismatch"

applyKs :: C.Kind -> [C.Kind] -> TE C.Kind
applyKs = foldM applyK

translateClassDef' :: KindSubst -> InterfaceDef -> TE ClassDef
translateClassDef' ks (InterfaceDef i ps v ms) = undefined

topoDatatypeDefs :: [DataTypeDef] -> Either String [DataTypeDef]
topoDatatypeDefs dtds =
  let graph                            = mkNode <$> dtds
      mkNode d@(DataTypeDef name _ ps) = ((name, deps ps), d)
      deps ps                          = mconcat $ dep <$> ps
      dep (ProductDef _ ts)            = filter isUserDefined $ filter isConstr $ mconcat $ deepflat <$> ts
      isConstr                         = isLetter . head
      isUserDefined                    = not . flip elem ["Int", "String"]
      deepflat (TCon c)                = [c]
      deepflat (TApp l r)              = deepflat l <> deepflat r
      deepflat _                       = []
   in case topo graph of
        Nothing   -> Left "Missing data type..."
        Just ords -> return $ snd <$> ords

