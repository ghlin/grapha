module Pipes.Lifter
  ( liftCombinators
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Writer    as W
import           Control.Monad.Trans.State     as T
import           Control.Monad.Trans.Except    as E
import           Data.List                      ( union
                                                , (\\)
                                                )
import           Pipes.ConstrCollector
import           Lang.Core
import           Lang.SC
import           Misc

liftCombinators :: [(Name, Int)] -> ConstrsTable -> [CoreCombinator] -> Either ErrorMessage SCProgram
liftCombinators bs ct cs = do let initialState = LState 0 bs ct
                              let (scs, e) = runL (liftAll cs) initialState
                              s <- e
                              case lookup "main" s of
                                Nothing -> Left "no main combinator found"
                                Just n  -> return (fixToplevelNames s <$> scs, n)

liftAll :: [CoreCombinator] -> L [(Name, Name)]
liftAll cs = do let ns  = fmap (\(CoreCombinator n _ _) -> n) cs
                ks <- mapM (liftC ns) cs
                return $ ns `zip` ks

data LState
  = LState
    { supply   :: Int
    , builtins :: [(Name, Int)]
    , constrs  :: ConstrsTable
    }

-- the lifting monad
type L a = E.ExceptT ErrorMessage
         ( W.WriterT [SC]
         ( T.State LState
         )) a

runL :: L a -> LState -> ([SC], Either ErrorMessage a)
runL m s = let uw1 = E.runExceptT m
               uw2 = W.runWriterT uw1
               (e, scs) = T.evalState uw2 s
            in (scs, e)

acquireId :: String -> L Name
acquireId pref = do i <- (+ 1) <$> lift (lift $ T.gets supply)
                    lift $ lift $ T.modify $ \s -> s { supply = i }
                    return $ pref <> "{" <> show i <> "}"

getConstr :: Name -> L Constr
getConstr c = do cs <- lift $ lift $ T.gets constrs
                 case lookup c cs of
                   Just constr -> return constr
                   Nothing     -> E.throwE $ "FIXME [Lifter.getConstr]: missing constr: " <> c

arityConstr :: Name -> L Int
arityConstr = fmap cArity . getConstr

arityBuiltin :: Name -> L (Maybe Int)
arityBuiltin b = lift $ lift $ T.gets (lookup b . builtins)

data VarType
  = VTSym
  | VTCon
  | VTSC
  deriving (Eq)

typeOfVar :: Name -> VarType
typeOfVar ('$':_)        = VTSC
typeOfVar n | isConstr n = VTCon
typeOfVar _              = VTSym

liftE :: [Name] -> CoreExpr -> L (SCExpr, [Name])
liftE s (EVar n) = case typeOfVar n of
                     VTSym -> do b <- arityBuiltin n
                                 case b of
                                   Nothing -> return (SCVar n, [n] \\ s)
                                   Just a  -> return (SCBuiltin n a, [])
                     VTSC  -> return (SCVar n, [])
                     VTCon -> do arity <- arityConstr n
                                 return (SCPack n arity, [])
liftE _ (ELit l) = return (SCLit l, [])
liftE s (EApp l r) = do (l', fl) <- liftE s l
                        (r', fr) <- liftE s r
                        return (SCApp l' r', fl `union` fr)
liftE s (EIf c t e) = do (c', fc) <- liftE s c
                         (t', ft) <- liftE s t
                         (e', fe) <- liftE s e
                         return (SCIf c' t' e', fc `union` ft `union` fe)
liftE s (ETest n e) = do (e', fe) <- liftE s e
                         return (SCApp (SCTest n) e', fe)
liftE s (EPick _ f e) = do (e', fe) <- liftE s e
                           return (SCApp (SCPick f) e', fe)
liftE s (ELam ps e) = do (e', fe) <- liftE s e
                         let extraVars = fe \\ (s `union` ps)
                         name <- acquireId "$SC_LAM"
                         let sc = SC name (extraVars <> ps) e'
                         lift $ W.tell [sc]
                         let partial = foldl SCApp (SCVar name) (SCVar <$> extraVars)
                         return (partial, extraVars)
liftE s (ELet [CoreCombinator f [] e] body) = do let s' = f:s
                                                 (e', fe) <- liftE s e
                                                 (b', fb) <- liftE s' body
                                                 let frs = (fe `union` fb) \\ [f]
                                                 if f `elem` fe -- recursive binding
                                                    then return (SCLetRec [(f, e')] b', frs)
                                                    else return (SCLet f e' b', frs)
liftE s (ELet [CoreCombinator f ps e] body) = liftE s $ ELet [CoreCombinator f [] $ ELam ps e] body
liftE s (ELet bs body) = do let ps = combinatorToPair <$> bs
                            let fns  = fst <$> ps  -- binding左侧名字
                            let s' = s `union` fns
                            rps  <- forM ps $ \(fn, fe) -> liftE (fn:s) fe
                            let rhss = fst <$> rps -- binding右侧表达式
                            let fvss = snd <$> rps -- 自由变量
                            (body', fvbs) <- liftE s' body
                            let frvs = foldr union fvbs fvss \\ fns
                            return (SCLetRec (fns `zip` rhss) body', frvs)

liftC :: [Name] -> CoreCombinator -> L Name
liftC s (CoreCombinator f vs b) = do name <- acquireId $ '$':f
                                     b' <- subst f (SCVar name) . fst <$> liftE s b
                                     let sc = SC name vs b'
                                     lift $ W.tell [sc]
                                     return name

fixToplevelNames :: [(Name, Name)] -> SC -> SC
fixToplevelNames ks (SC name args body) = SC name args $ foldr sub body ks
  where sub (k, n) = subst k (SCVar n)

combinatorToPair :: CoreCombinator -> (Name, CoreExpr)
combinatorToPair (CoreCombinator f [] e) = (f, e)
combinatorToPair (CoreCombinator f ps e) = (f, ELam ps e)

subst :: Name -> SCExpr -> SCExpr -> SCExpr
subst v k (SCVar v') | v == v'     = k
subst v k (SCApp l r )             = SCApp (subst v k l) (subst v k r)
subst v k (SCIf c t e)             = SCIf (subst v k c) (subst v k t) (subst v k e)
subst v k (SCLet v' e b) | v /= v' = SCLet v' (subst v k e) (subst v k b)
subst v k (SCLetRec bs e) | v `notElem` (fst <$> bs) =
  SCLetRec ((fst <$> bs) `zip` (subst v k . snd <$> bs)) (subst v k e)
subst _ _ e = e

