module Lang.Surface.Balancer
  ( balance
  ) where

import           Lang.Surface
import           Misc

type InfixDatabase = [(Name, (InfixAssoc, Int))]

shouldRotate' :: Int -> InfixAssoc -> Int -> Bool
shouldRotate' lp la rp = lp > rp || (lp == rp && la == AssocRight)

data AssocStatus = AShouldRotate | AMissAssoc | AOk deriving (Eq)

defaultAssoc :: (InfixAssoc, Int)
defaultAssoc = (AssocLeft, 5)

checkAssoc :: InfixDatabase -> Name -> Name -> AssocStatus
checkAssoc db l r = let (la, lp) = maybe defaultAssoc id $ lookup l db
                        (ra, rp) = maybe defaultAssoc id $ lookup r db
                     in if shouldRotate' lp la rp then AShouldRotate else AOk
                     -- FIXME: check missassoc (la == ra == None ?)

balance' :: InfixDatabase -> Expression -> Expression
balance' db x = r x
  where
    r (ELam  p e        ) = EIndir $ ELam p $ r e
    r (EApp  f e        ) = EIndir $ EApp (r f) (r e)
    r (ECase e as       ) = EIndir $ ECase (r e) $ mapE r <$> as
    r (EIf c t e        ) = EIndir $ EIf (r c) (r t) (r e)
    r (ELet bs e        ) = EIndir $ ELet (mapE r <$> bs) (r e)
    r (EDo ss           ) = EIndir $ EDo $ mapE r <$> ss
    r (EUnary n e       ) = EIndir $ EUnary n $ r e
    r (EListLiteral  es ) = EIndir $ EListLiteral $ r <$> es
    r (ETupleLiteral es ) = EIndir $ ETupleLiteral $ r <$> es
    r (EQuoted       e  ) = EIndir $ EQuoted $ r e
    r (EBinary n lhs rhs) = r' $ EBinary n (r lhs) rhs
    r e                   = e
    r' e@(EBinary α (EBinary β a b) c) | checkAssoc db α β == AShouldRotate = EBinary β a $ r $ EBinary α b c
    r' e = e

-- | 去除EIndir和EQuoted
simplify :: Expression -> Expression
simplify (EIndir  x       ) = simplify x
simplify (EQuoted x       ) = simplify x
simplify (ELam  p e       ) = ELam p $ simplify e
simplify (EApp  p e       ) = EApp (simplify p) (simplify e)
simplify (ECase e as      ) = ECase (simplify e) $ mapE simplify <$> as
simplify (EIf c t e       ) = EIf (simplify c) (simplify t) (simplify e)
simplify (ELet bs e       ) = ELet (mapE simplify <$> bs) (simplify e)
simplify (EDo ss          ) = EDo $ mapE simplify <$> ss
simplify (EUnary n e      ) = EUnary n $ simplify e
simplify (EBinary n l r   ) = EBinary n (simplify l) (simplify r)
simplify (EListLiteral  es) = EListLiteral $ simplify <$> es
simplify (ETupleLiteral es) = ETupleLiteral $ simplify <$> es
simplify e                  = e

-- | 先前parsing的过程将所有二元运算符看做了左结合,优先级5
-- 在这里把二元运算的结合顺序恢复为正确的顺序
balance :: [InfixDef] -> Expression -> Expression
balance ids = simplify . balance' (translateDB ids)
  where translateDB                          = fmap translate
        translate (InfixDef name prec assoc) = (name, (assoc, prec))

