{-# LANGUAGE TupleSections #-}
module Lang.Typing where

import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.List                      ( union
                                                , intersect
                                                , nub
                                                )
import           Data.Char                      ( isLetter )
import           Control.Monad                  ( msum
                                                , mapM
                                                )
import           Control.Monad.Trans.Except    as E
import           Control.Monad.Trans.State     as T
import           Control.Monad.Trans.Class      ( lift )
import qualified Lang.Surface                  as S
import           Lang.Core
import           Misc

class HasKind t where
  kind :: t -> Kind

instance HasKind TyVar where
  kind (TyVar _ k) = k

instance HasKind TyCon where
  kind (TyCon _ k) = k

instance HasKind Ty where
  kind (TVar v) = kind v
  kind (TCon c) = kind c
  kind (TApp t _) = case kind t of KFun _ k -> k

-- | id -> type substitution
type Subst = [(TyVar, Ty)]

-- | the empty substitution, which substitutes nothing
empty :: Subst
empty = []

-- | assume that u is of type t
assume :: TyVar -> Ty -> Subst
assume u t = singleton (u, t)

class Types t where
  apply :: Subst -> t -> t
  frees :: t -> [TyVar]

instance Types Ty where
  apply s (TVar u)   = fromMaybe (TVar u) $ lookup u s
  apply s (TApp l r) = TApp (apply s l) (apply s r)
  apply _ t          = t

  frees (TVar u)   = [u]
  frees (TApp l r) = frees l `union` frees r
  frees _          = []

instance Types a => Types [a] where
  apply s = fmap (apply s)
  frees   = nub . mconcat . fmap frees

instance Types Pred where
  apply s (Pred iface t) = Pred iface $ apply s t
  frees (Pred _ t)       = frees t

instance Types t => Types (Qual t) where
  apply s (Qual ps t) = Qual (apply s ps) (apply s t)
  frees (Qual ps t)   = frees ps `union` frees t

instance Types Sc where
  apply s (Sc ks q) = Sc ks $ apply s q
  frees (Sc _ q)    = frees q

infixr 4 @@

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] <> s1

infix 4 @@!

(@@!) :: Monad m => Subst -> Subst -> m Subst
s1 @@! s2 = if ok then return $ s1 <> s2 else fail "merge conflict"
  where ok     = all pass $ fmap fst s1 `intersect` fmap fst s2
        pass v = apply s1 (TVar v) == apply s2 (TVar v)

-- | the most general unifier
mgu :: Monad m => Ty -> Ty -> m Subst
mgu (TApp l r) (TApp l' r') = do s1 <- mgu l l'
                                 s2 <- mgu r r'
                                 return $ s2 @@ s1
mgu (TVar u) t                     = bindVar u t
mgu t        (TVar u)              = bindVar u t
mgu (TCon c1) (TCon c2) | c1 == c2 = return empty
mgu _ _                            = fail "Types doesn't unify"

bindVar :: Monad m => TyVar -> Ty -> m Subst
bindVar u t | t == TVar u      = return empty
            | u `elem` frees t = fail "Occurs check fails (infinate type)"
            | kind u /= kind t = fail "Kind mismatch"
            | otherwise        = return $ assume u t

-- | like mgu, but only binds the left hand operand
match :: Monad m => Ty -> Ty -> m Subst
match (TApp l r) (TApp l' r') = do s1 <- match l l'
                                   s2 <- match r r'
                                   s1 @@! s2
match (TVar u) t | kind u == kind t  = return $ assume u t
match (TCon c1) (TCon c2) | c1 == c2 = return empty
match _ _                            = fail "Types doesn't unify"

-- | like mgu and match, but on predictions
mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred   = liftP mgu
matchPred = liftP match

liftP m (Pred i t) (Pred i' t') | i == i'   = m t t'
                                | otherwise = fail "Class mismatch"

-- | Class, (<super classes>, <instances>)
type Class = ([Name], [Inst])

type Inst  = Qual Pred

data ClassEnv
  = ClassEnv
    { classes :: [(Name, Class)]
    }

-- | get all superclasses
supers :: ClassEnv -> Name -> [Name]
supers (ClassEnv cs) i = maybe [] fst $ lookup i cs

-- | get all instances
insts :: ClassEnv -> Name -> [Inst]
insts (ClassEnv cs) i = maybe [] snd $ lookup i cs

-- entailment...

-- | 为检查给定pred能否满足,检查可用pred的所有父类
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(Pred iface t) = p:mconcat [bySuper ce (Pred i' t) | i' <- supers ce iface]

-- | 为了能够在Pred下满足inst,需要满足哪些Pred
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(Pred i t) = msum $ tryInst <$> insts ce i
  where tryInst (Qual ps h) = flip apply ps <$> matchPred h p

-- | 检查在某些[pred]成立的情况下,给定的Pred能否被满足
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (elem p) (bySuper ce <$> ps)               -- ^ 从已满足pred的父类中找到了p
              || maybe False (all $ entail ce ps) (byInst ce p) -- ^ 环境中存在满足p的某个实例

-- | 移除不必要的pred
simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where loop rs [] = rs
        loop rs (p:ps) | entail ce (rs <> ps) p = loop rs ps -- 如果rs和ps已经足够推导出p,那么p是多余的
                       | otherwise = loop (p:rs) ps

-- | 化简
reduce :: ClassEnv -> [Pred] -> Maybe [Pred]
reduce ce ps = simplify ce <$> toHnfs ce ps
  where toHnf ce p | inHnf p   = return [p]
                   | otherwise = byInst ce p >>= toHnfs ce
        toHnfs ce ps     = mconcat <$> mapM (toHnf ce) ps
        inHnf (Pred c t) = hnf t
        hnf TVar{}       = True
        hnf TCon{}       = False
        hnf (TApp t _)   = hnf t

-- | register cllass to env
register :: ClassEnv -> Name -> Class -> ClassEnv
register (ClassEnv cs) i c = ClassEnv $ (i, c):cs

-- | check if given class is defined
defined :: Name -> ClassEnv -> Bool
defined i (ClassEnv cs) = isJust $ lookup i cs

initialEnv :: ClassEnv
initialEnv = ClassEnv []

-- | helpers for adding class to env
type EnvTrans = ClassEnv -> Maybe ClassEnv

addClass :: Name -> [Name] -> EnvTrans
addClass i scs ce | defined i ce                    = fail $ "Class " <> i <> " has already been defined"
                  | any (not . flip defined ce) scs = fail "Superclass not defined"
                  | otherwise                       = return $ register ce i (scs, [])

addInst :: [Pred] -> Pred -> EnvTrans
addInst ps p@(Pred iface t) ce | not (defined iface ce) = fail "No class for instance"
                               | any (overlap p) qs     = fail "Overlapped instance"
                               | otherwise              = return $ register ce iface (supers ce iface, Qual ps p:its)
                               where its = insts ce iface
                                     qs  = [q | Qual _ q <- its]

overlap :: Pred -> Pred -> Bool
overlap p q = isJust $ mguPred p q

-- | lift to scheme
quantify :: [TyVar] -> Qual Ty -> Sc
quantify vs q = Sc ks (apply s q)
  where vs' = [ v | v <- frees q, v `elem` vs]
        ks  = kind <$> vs'
        s   = zip vs' (TGen <$> [0..])

toSc :: Ty -> Sc
toSc = Sc [] . Qual []

-- | Assumption, id -> ty
data Assump = Assump Name Sc

instance Types Assump where
  apply s (Assump i sc) = Assump i $ apply s sc
  frees (Assump _ sc)   = frees sc

lookupAssump :: Monad m => Name -> [Assump] -> m Sc
lookupAssump i [] = fail $ "Unbound identifier: " <> i
lookupAssump i (Assump i' sc : as) | i == i'   = return sc
                                   | otherwise = lookupAssump i as

-- | for now...
type TypingError = String

data TypingState
  = TypingState
    { supply :: Int
    , substs :: Subst
    , symtab :: [(Name, Sc)] -- ^ 已知的符号的类型 TODO: 应该放在ReaderT内
    }

-- the type inference monad.
type Typing a = E.ExceptT TypingError (T.State TypingState) a

runTyping :: [(Name, Sc)] -> Typing a -> Either TypingError a
runTyping sc = flip T.evalState (initialState sc) . E.runExceptT

initialState :: [(Name, Sc)] -> TypingState
initialState cs = TypingState 0 empty cs

lookupSymSc :: Name -> Typing Sc
lookupSymSc i = do vcs <- lift $ T.gets symtab
                   case lookup i vcs of
                     Just sc -> return sc
                     Nothing -> E.throwE $ "Invalid SymSc: " <> i

getSubst :: Typing Subst
getSubst = lift $ T.gets substs

extendSubst :: Subst -> Typing ()
extendSubst s = lift $ T.modify $ \env -> env { substs = s @@ substs env }

unify :: Ty -> Ty -> Typing ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extendSubst u

newTVar :: Kind -> Typing Ty
newTVar k = do
  i <- (+1) <$> lift (T.gets supply)
  let name = "ty_" <> show i
  lift $ T.modify $ \env -> env { supply = i }
  return $ TVar $ TyVar name k

instSc :: Sc -> Typing (Qual Ty)
instSc (Sc ks q) = flip instantiate q <$> mapM newTVar ks
-- where instantiate is defined as:

class Instantiate t where
  instantiate :: [Ty] -> t -> t

instance Instantiate Ty where
  instantiate ts (TApp l r) = TApp (instantiate ts l) (instantiate ts r)
  instantiate ts (TGen n)   = ts !! n
  instantiate _  t          = t

instance Instantiate t => Instantiate [t] where
  instantiate = fmap . instantiate

instance Instantiate Pred where
  instantiate ts (Pred i t) = Pred i $ instantiate ts t

instance Instantiate t => Instantiate (Qual t) where
  instantiate ts (Qual ps t) = instantiate ts ps `Qual` instantiate ts t

-- | infer literals
inferL :: S.Literal -> Typing ([Pred], Ty)
interL S.LInteger{} = return ([], tInt)
inferL S.LString{}  = return ([], tString)
inferL _            = E.throwE "FIXME: Unimplemented"

-- | infer patterns
inferP :: S.Pattern -> Typing ([Pred], [Assump], Ty)
inferP (S.PVar v) = do t <- newTVar KStar
                       return ([], [Assump v $ toSc t], t)
inferP S.PWildcard = ([], [],) <$> newTVar KStar
inferP (S.PLit l) = do (ps, t) <- inferL l
                       return (ps, [], t)
inferP (S.PCon c pats) = do (ps, as, ts) <- inferPs pats
                            t' <- newTVar KStar
                            Qual qs t <- lookupSymSc c >>= instSc
                            unify t (foldr fn t' ts)
                            return (ps <> qs, as, t')

inferPs :: [S.Pattern] -> Typing ([Pred], [Assump], [Ty])
inferPs pats = do s3 <- mapM inferP pats
                  let ps = mconcat [ s | (s, _, _) <- s3 ]
                  let as = mconcat [ s | (_, s, _) <- s3 ]
                  let ts =         [ s | (_, _, s) <- s3 ]
                  return (ps, as, ts)

-- common signature for type inference
type Infer e t = ClassEnv -> [Assump] -> e -> Typing ([Pred], t)

isConstr :: Name -> Bool
isConstr = isLetter . head

inferE :: Infer S.Expression Ty
inferE ce as (S.EVar i) = do Qual ps t <- (if isConstr i then lookupSymSc i else lookupAssump i as) >>= instSc
                             return (ps, t)
inferE ce as (S.ELit l) = inferL l
inferE ce as (S.EApp l r) = do (ps, tl) <- inferE ce as l
                               (qs, tr) <- inferE ce as r
                               t <- newTVar KStar
                               -- l   :: a -> b
                               -- r   :: a
                               -- l r :: b
                               unify tl (tr `fn` t)
                               return (ps <> qs, t)
infer ce as (S.ELet bs e) = E.throwE "FIXME: Unimplemented"

