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

builtinTys :: [(Name, Type)]
builtinTys = fmap (\(a, _, c) -> (a, c)) builtinCombinatorSignatures

makeToplevelLet :: [[CoreCombinator]] -> CoreExpr
makeToplevelLet = L.foldr ELet (EVar "main")

nameOf :: CoreCombinator -> Name
nameOf (CoreCombinator name _ _) = name

infer :: ConstrsTable -> Pipe ErrorMessage [[CoreCombinator]] Type
infer cs grs = runTI cs $ do ass  <- deriveAssumps cs
                             ass' <- builtinAssumps ass builtinTys
                             inferE ass' $ makeToplevelLet grs

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
    { supply  :: Int
    , substs  :: Subst
    , constrs :: ConstrsTable
    , which   :: String
    }

type TI a = E.ExceptT ErrorMessage (T.State TState) a

runTI :: ConstrsTable -> TI a -> Either ErrorMessage a
runTI cs = flip T.evalState (TState 0 noSubst cs "") . E.runExceptT

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
getSubst = lift $ T.gets substs

extSubst :: Subst -> TI ()
extSubst s = lift $ T.modify $ \env -> env { substs = s @@ substs env }

getEnclosingCombinators :: TI String
getEnclosingCombinators = lift $ T.gets which

setEnclosingCombinators :: String -> TI ()
setEnclosingCombinators path = lift $ T.modify $ \s -> s { which = path }

pushCombinatorName :: Name -> TI ()
pushCombinatorName name = do sofar <- getEnclosingCombinators
                             setEnclosingCombinators $ if L.null sofar
                                                          then name
                                                          else sofar <> " > " <> name

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
mgu t1 t2 = do enclosing <- getEnclosingCombinators
               E.throwE $ "Type doesn't unify:"
                       <> "\n     type: "
                       <> prettyTy t1
                       <> "\n and type: "
                       <> prettyTy t2
                       <> "\nIn combinator: "
                       <> enclosing

bindVar :: Name -> Type -> TI Subst
bindVar u t | t == TVar u = return noSubst
            | u `elem` frees t = do enclosing <- getEnclosingCombinators
                                    E.throwE $ "Occurs check failed (infinate type)"
                                            <> "In combinator: " <> enclosing
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

refresh :: Type -> TI Type
refresh t = do s <- getSubst
               return $ apply s t

inferL :: Literal -> TI Type
inferL LInteger {} = return tInt
inferL LString {}  = return tString
inferL LChar {}    = return tChar
inferL _           = E.throwE "TODO: unimplemented literal type"

inferE :: Assumptions -> CoreExpr -> TI Type
inferE _ (ELit l) = inferL l
inferE (Assumptions env) (EVar v) = case M.lookup v env of
                                      Just t  -> instantiate t -- monotype
                                      Nothing -> do enclosing <- getEnclosingCombinators
                                                    E.throwE $ "Use of undefined name: " <> v
                                                            <> "\nIn combinator: " <> enclosing
inferE ass (ELam vs body) = inferLam ass vs body
inferE ass (EApp f x) = do t  <- TVar <$> acquireId "a"
                           t1 <- inferE ass f
                           t2 <- inferE ass x
                           unify t1 (t2 `fn` t)
                           refresh t
inferE ass (EIf c t e) = do ct <- inferE ass c
                            tt <- inferE ass t
                            et <- inferE ass e
                            unify ct tBool
                            unify tt et
                            refresh tt
inferE ass (ETest c e) = do dsc <- cDataScheme <$> getConstr c
                            dt  <- datatype dsc
                            t   <- inferE ass e
                            unify t dt
                            return tBool
inferE ass (EPick c f e) = do constr <- getConstr c
                              ct <- component $ cComponents constr !! f
                              dt <- datatype $ cDataScheme constr
                              e  <- inferE ass e
                              unify dt e
                              refresh ct
inferE ass (ELet bs body) = do let bindings = destruct <$> bs
                               let lhss = fst <$> bindings
                               tyLhss <- fmap TVar <$> mapM acquireId lhss
                               let image a (name, ty) = a `extAssump` (name, generalize a ty)
                               let ass' = L.foldl image ass (lhss `zip` tyLhss)
                               let rhss = snd <$> bindings
                               let rhssChecks = lhss `zip` tyLhss `zip` rhss
                               let checkRHS a ((lhs, tyLhs), rhs) = do
                                     save <- getEnclosingCombinators
                                     pushCombinatorName lhs
                                     tyRhs <- inferE a rhs
                                     unify tyLhs tyRhs
                                     setEnclosingCombinators save
                                     return $ a `extAssump` (lhs, generalize a tyRhs) -- override previous guesses

                               ass''  <- foldM checkRHS ass'  rhssChecks

                               -- double check... not sure if this
                               -- is necessary
                               ass''' <- foldM checkRHS ass'' rhssChecks

                               inferE ass''' body >>= refresh


destruct :: CoreCombinator -> (Name, CoreExpr)
destruct (CoreCombinator name [] body)   = (name, body)
destruct (CoreCombinator name args body) = (name, ELam args body)

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

