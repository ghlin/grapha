{-# LANGUAGE TupleSections #-}
module Lang.TypingEnv where

import Debug.Trace

import           Control.Applicative            ( (<|>) )
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
                    Nothing -> Left $ "Unbound: " <> i <> ", ks: " <> show ks

extendKS :: Name -> C.Kind -> KindSubst -> TE KindSubst
extendKS n k ks = case lookup n ks of
                    Nothing -> return $ (n, k):ks
                    Just k' -> if k == k' then return ks else Left $ "Kind mismatch: " <> n

-- | 断言:Type具有Kind: KStar
fromType :: KindSubst -> KindSubst -> Type -> TE KindSubst
fromType cks vks t = fromType' cks vks C.KStar t

fromType' :: KindSubst -> KindSubst -> C.Kind -> Type -> TE KindSubst
fromType' cks vks k t = fromTypes cks vks k $ flatten t

-- | [Type]由某个Type经过flatten得到,这里断言那个Type具有给定的Kind
-- 在这个假设之下,推断Type内部符号的Kind
fromTypes :: KindSubst -> KindSubst -> C.Kind -> [Type] -> TE KindSubst
fromTypes cks vks k ts = syn vks k ts
  where syn  vks k (TVar f:rs) = do let ks' = replicate (length rs) C.KStar
                                    vks' <- synthsis cks vks ks' rs
                                    let k' = foldr C.KFun k ks'
                                    extendKS f k' vks'
        syn  vks k (TCon c:rs) = do ck <- lookupKind c cks
                                    let ks' = flattenK' ck
                                    vks' <- synthsis cks vks ks' rs
                                    k' <- applyKs ck ks'
                                    if k /= k'
                                       then Left $ "Kind mismatch: " <> show k <> " / " <> show k'
                                       else return vks'

-- | 合成ts 和 ks
synthsis :: KindSubst -> KindSubst -> [C.Kind] -> [Type] -> TE KindSubst
synthsis _ _ ks ts | length ks /= length ts = Left $ "Kind mismatch (length differ)\n" <> "l: " <> show ks <> "\nr: " <> show ts
synthsis cks vks ks ts                      = s $ ks `zip` ts
  where s              = foldM syn vks
        syn vks (k, t) = fromTypes cks vks k $ flatten t

fromProd :: KindSubst -> [Name] -> ProductDef -> TE KindSubst
fromProd ks vs (ProductDef _ ts) = do vks <- foldM ft [] ts
                                      return $ filter (onlyVs . fst) vks
                                      where ft vs t  = case fromType ks vs t of
                                                         Right vs' -> return vs'
                                                         _         -> return vs
                                            onlyVs n = n `elem` vs

dataTypeKinds :: KindSubst -> DataTypeDef -> TE KindSubst
dataTypeKinds ks (DataTypeDef name as ps) = do as' <- mconcat <$> mapM (fromProd ks as) ps
                                               kas <- mapM (flip lookupKind as') as
                                               extendKS name (foldr C.KFun C.KStar kas) ks

fromPred :: KindSubst -> KindSubst -> Pred -> TE KindSubst
fromPred cks vks (Pred i t) = do k <- lookupKind i cks
                                 fromType' cks vks k t

fromAnnot :: Name -> KindSubst -> KindSubst -> CombinatorAnnot -> TE KindSubst
fromAnnot v cks vks (CombinatorAnnot _ (Qual ps t)) = do vks' <- foldM (fromPred cks) vks ps
                                                         -- t is of kind *
                                                         vks'' <- fromType' cks vks' C.KStar t
                                                         return $ filter ((== v) . fst) vks''

classKinds :: KindSubst -> InterfaceDef -> TE KindSubst
classKinds ks (InterfaceDef name ps a as) = do ks' <- foldM (fromPred ks) [] ps
                                               vks <- foldM (fromAnnot a ks) ks' as
                                               k   <- lookupKind a vks
                                               extendKS name k ks

data ClassDef = ClassDef Name [C.Pred] C.TyVar
  deriving (Show, Eq)

data InstDef = InstDef Name [C.Pred] C.Ty
  deriving (Show, Eq)

flatten :: Type -> [Type]
flatten (TApp l r) = flatten l <> [r]
flatten v          = [v]

flattenK :: C.Kind -> [C.Kind]
flattenK (C.KFun l r) = [l] <> flattenK r
flattenK k            = [k]

flattenK' :: C.Kind -> [C.Kind]
flattenK' = reverse . tail . reverse . flattenK

applyK :: C.Kind -> C.Kind -> TE C.Kind
applyK (C.KFun k1 k2) k | k1 == k = return k2
applyK _              _           = Left "Kind mismatch"

applyKs :: C.Kind -> [C.Kind] -> TE C.Kind
applyKs = foldM applyK

translatePred :: KindSubst -> KindSubst -> Pred -> TE C.Pred
translatePred cks vks (Pred i t) = do k <- lookupKind i cks
                                      t' <- translateType cks vks k t
                                      return $ C.Pred i t'

translateType' :: KindSubst -> KindSubst -> Type -> TE C.Ty
translateType' cks vks = t
  where t (TApp l r) = C.TApp <$> (t l) <*> (t r)
        t (TCon c)   = (C.TCon . C.TyCon c) <$> lookupKind c cks
        t (TVar v)   = (C.TVar . C.TyVar v) <$> lookupKind v vks

translateType :: KindSubst -> KindSubst -> C.Kind -> Type -> TE C.Ty
translateType cks vks k t = do vks' <- fromType' cks vks k t
                               translateType' cks vks' t

translateClassDef :: KindSubst -> InterfaceDef -> TE ClassDef
translateClassDef ks (InterfaceDef i ps v _) = do k   <- lookupKind i ks
                                                  ps' <- mapM (translatePred ks $ singleton (v, k)) ps
                                                  tv  <- C.TyVar v <$> lookupKind i ks
                                                  return $ ClassDef i ps' tv

translateInstDef :: KindSubst -> InstanceDef -> TE InstDef
translateInstDef ks (InstanceDef i ps ty _) = do k   <- lookupKind i ks
                                                 ps' <- mapM (translatePred ks []) ps
                                                 tv  <- translateType ks [] k ty
                                                 return $ InstDef i ps' tv


-- | 按照依赖顺序排序所有Interface
topoClassDefs :: [InterfaceDef] -> TE [InterfaceDef]
topoClassDefs ifds =
  let graph                             = mkNode <$> ifds
      mkNode d@(InterfaceDef i ps _ _)  = ((i, deps ps), d)
      deps                              = fmap dep
      dep (Pred i _)                    = i
   in case topo graph of
        Nothing   -> Left "Missing class def..."
        Just ords -> return $ snd <$> ords

-- | 按照依赖顺序排序所有DataTypeDef
topoDatatypeDefs :: [DataTypeDef] -> TE [DataTypeDef]
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

-- | default bindings
initialSubsts :: KindSubst
initialSubsts = [ ("Int", C.KStar)
                , ("String", C.KStar)
                , ("()", C.KStar)
                , ("->", C.fromArity 2)
                , ("[]", C.fromArity 1)
                ] <> [ (tupleCon n, C.fromArity n) | n <- [2..4] ]

