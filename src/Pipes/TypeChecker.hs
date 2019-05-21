module Pipes.TypeChecker where

import           Control.Monad.Trans.Except    as E
import           Control.Monad.Trans.State     as T
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad
import           Data.Maybe                     ( fromMaybe )
import           Data.Map                      as M
import           Data.List                     as L
import           Lang.Type                     as T
import           Lang.Literal
import           Lang.Core
import           Lang.Builtins
import           Pipes.ConstrCollector
import           Misc
import           Pipe

import           Debug.Trace

builtinTys :: [(Name, Type)]
builtinTys = fmap (\(a, _, c) -> (a, c)) builtinCombinatorSignatures

infer :: ConstrsTable -> Pipe ErrorMessage [CoreCombinator] ()
infer cs b = runTI cs $ do ass  <- deriveAssumps cs
                           ass' <- builtinAssumps ass builtinTys
                           let inferC' ass b = fst <$> inferC ass b
                           foldM inferC' ass' b
                           return ()

type Subst = Map Name Type

noSubst :: Subst
noSubst = empty

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = (M.map (apply s1) s2) `M.union` s1

-- | where substitutions occurs
class Types a where
  frees :: a -> [Name]
  apply :: Subst -> a -> a

instance Types t => Types [t] where
  frees = L.foldr L.union [] . fmap frees
  apply = fmap . apply

instance Types Type where
  frees = tFVs
  apply s (TVar v)   = fromMaybe (TVar v) $ M.lookup v s
  apply s (TApp f a) = TApp (apply s f) (apply s a)
  apply _ c          = c

instance Types Scheme where
  frees (Forall as ty) = frees ty L.\\ as
  apply s (Forall as ty) = Forall as $ apply (L.foldr M.delete s as) ty

data TState
  = TState
    { supply :: Int
    , substs :: Subst
    , constrs :: ConstrsTable
    }

type TI a = E.ExceptT ErrorMessage (T.State TState) a

runTI :: ConstrsTable -> TI a -> Either ErrorMessage a
runTI cs = flip T.evalState (TState 0 noSubst cs) . E.runExceptT

acquireId :: String -> TI Name
acquireId pref = do i <- (+ 1) <$> lift (T.gets supply)
                    lift $ T.modify $ \s -> s { supply = i }
                    return $ pref <> "{ty_" <> show i <> "}"

getConstr :: Name -> TI Constr
getConstr n = do cs <- lift $ T.gets constrs
                 case Prelude.lookup n cs of
                   Nothing -> E.throwE $ "Unbound constructor: " <> n
                   Just c  -> return c

getSubst :: TI Subst
getSubst = do lift $ T.gets substs

extSubst :: Subst -> TI ()
extSubst s = lift $ T.modify $ \env -> env { substs = s @@ substs env }

unify :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

mgu :: Type -> Type -> TI Subst
mgu (TApp l r) (TApp l' r') = do s1 <- mgu l l'
                                 s2 <- mgu r r'
                                 return $ s2 @@ s1
mgu (TVar u) t                     = bindVar u t
mgu t        (TVar u)              = bindVar u t
mgu (TCon c1) (TCon c2) | c1 == c2 = return noSubst
mgu t1 t2 = E.throwE $ "Type doesn't unify:" <> "\nt1 = " <> show t1 <> "\nt2 = " <> show t2

bindVar :: Name -> Type -> TI Subst
bindVar u t | t == TVar u = return noSubst
            | u `elem` frees t = E.throwE "Occurs check failed (infinate type)"
            | otherwise = return $ M.singleton u t

newtype Assumptions
  = Assumptions { unAssump :: M.Map Name Scheme }

extAssump :: Assumptions -> (Name, Scheme) -> Assumptions
extAssump (Assumptions a) (v, s) = Assumptions $ M.delete v a `M.union` M.singleton v s

instance Types Assumptions where
  frees   = frees . M.elems . unAssump
  apply s = Assumptions . M.map (apply s) . unAssump

generalize :: Assumptions -> Type -> Scheme
generalize a t = Forall (frees t L.\\ frees a) t

instantiate :: Scheme -> TI Type
instantiate (Forall as ty) = do vs' <- mapM (fmap TVar . acquireId) as
                                return $ apply (M.fromList $ as `zip` vs') ty

inject :: Constr -> TI (Name, Scheme)
inject c = do let name = cName c
              let sc   = cScheme c
              sc' <- constr sc
              return (name, sc')

deriveAssumps :: ConstrsTable -> TI Assumptions
deriveAssumps ct = do ps <- mapM inject (snd <$> ct)
                      return $ L.foldl extAssump (Assumptions M.empty) ps

builtinAssumps :: Assumptions -> [(Name, Type)] -> TI Assumptions
builtinAssumps ass ps = do let names = fst <$> ps
                           let tys = snd <$> ps
                           scs <- mapM builtinScheme tys
                           return $ L.foldl extAssump ass (names `zip` scs)

inferLam :: Assumptions -> [Name] -> CoreExpr -> TI Type
inferLam a [x] body = do x' <- TVar <$> acquireId "x"
                         let a' = a `extAssump` (x, Forall [] x')
                         t <- inferE a' body
                         s <- getSubst
                         return $ T.fn (apply s x') t
inferLam a (x:xs) body = inferE a $ ELam [x] $ ELam xs body

inferL :: Literal -> TI Type
inferL LInteger {} = return tInt
inferL LString {}  = return tString
inferL _           = E.throwE "TODO: unimplemented literal type"

inferE :: Assumptions -> CoreExpr -> TI Type
inferE _ (ELit l) = inferL l
inferE (Assumptions env) (EVar v) = case M.lookup v env of
                                      Nothing -> E.throwE $ "Unbound var: " <> v
                                      Just t  -> instantiate t -- monotype
inferE ass (ELam vs body) = inferLam ass vs body
inferE ass (EApp f x) = do t <- TVar <$> acquireId "a"
                           t1 <- inferE ass f
                           t2 <- inferE ass x
                           unify t1 (t2 `fn` t)
                           s <- getSubst
                           return $ apply s t
inferE ass (EIf c t e) = do ct <- inferE ass c
                            tt <- inferE ass t
                            et <- inferE ass e
                            unify ct tBool
                            unify tt et
                            s <- getSubst
                            return $ apply s tt
inferE ass (ETest c e) = do dsc <- cDataScheme <$> getConstr c
                            dt  <- datatype dsc
                            t   <- inferE ass e
                            unify t dt
                            return tBool
inferE ass (EPick c f e) = do constr <- getConstr c
                              ct <- component $ cComponents constr !! f
                              dt <- datatype $ cDataScheme constr
                              e <- inferE ass e
                              unify dt e
                              s <- getSubst
                              return $ apply s ct
inferE ass (ELet [b@CoreCombinator {}] body) = do (ass', _) <- inferC ass b
                                                  inferE ass' body
inferE ass (ELet (b:bs) body) = inferE ass $ ELet [b] $ ELet bs body

inferC :: Assumptions -> CoreCombinator -> TI (Assumptions, Type)
inferC ass (CoreCombinator f ps body) = do t <- TVar <$> acquireId f
                                           let sc   = generalize ass t
                                           let ass' = ass `extAssump` (f, sc)
                                           let body' = if L.null ps then body else ELam ps body
                                           bt <- inferE ass' body'
                                           unify bt t
                                           s <- getSubst
                                           let t' = apply s t
                                           let tf = generalize (apply s ass') t'
                                           let ass'' = apply s $ ass' `extAssump` (f, tf)
                                           bt' <- inferE ass'' body'
                                           unify bt' t
                                           return (ass'', bt')

builtinScheme :: Type -> TI Scheme
builtinScheme ty = constr (Forall (tFVs ty) ty)

constr :: Scheme -> TI Scheme
constr (Forall as ty) = do vs <- mapM acquireId as
                           let s = M.fromList $ as `zip` (TVar <$> vs)
                           return $ Forall vs $ apply s ty

datatype :: Scheme -> TI Type
datatype sch = do Forall _ ty <- constr sch
                  return ty

component :: Type -> TI Type
component t = datatype $ Forall (frees t) t

