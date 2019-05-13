module Lang.Surface.Balance
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

balance' :: InfixDatabase -> ExpressionX a -> ExpressionX a
balance' db x = r x
  where
    r (ELam b p e)           = EIndir $ ELam b p $ r e
    r (EApp b f e)           = EIndir $ EApp b (r f) (r e)
    r (ECase b e as)         = EIndir $ ECase b (r e) $ mapE r <$> as
    r (EIf b c t e)          = EIndir $ EIf b (r c) (r t) (r e)
    r (ELet bs e)            = EIndir $ ELet (mapE r <$> bs) (r e)
    r (EDo b ss)             = EIndir $ EDo b $ mapE r <$> ss
    r (EUnary b n e)         = EIndir $ EUnary b n $ r e
    r (EListLiteral b es)    = EIndir $ EListLiteral b $ r <$> es
    r (ETupleLiteral b es)   = EIndir $ ETupleLiteral b $ r <$> es
    r (EQuoted b e)          = EIndir $ EQuoted b $ r e
    r (EBinary p n lhs rhs)  = r' $ EBinary p n (r lhs) rhs
    r e                      = e
    r' e@(EBinary _ α (EBinary _ β a b) c) -- ann must be nothing...
      | checkAssoc db α β == AShouldRotate = EBinary Nothing β a $ r $ EBinary Nothing α b c
    r' e = e

-- | 去除EIndir和EQuoted
simplify :: ExpressionX a -> ExpressionX a
simplify (EIndir x)            = simplify x
simplify (EQuoted ann x)       = mapAnnot (const ann) $ simplify x
simplify (ELam b p e)          = ELam b p $ simplify e
simplify (EApp b p e)          = EApp b (simplify p) (simplify e)
simplify (ECase b e as)        = ECase b (simplify e) $ mapE simplify <$> as
simplify (EIf b c t e)         = EIf b (simplify c) (simplify t) (simplify e)
simplify (ELet bs e)           = ELet (mapE simplify <$> bs) (simplify e)
simplify (EDo b ss)            = EDo b $ mapE simplify <$> ss
simplify (EUnary b n e)        = EUnary b n $ simplify e
simplify (EBinary b n l r)     = EBinary b n (simplify l) (simplify r)
simplify (EListLiteral b es)   = EListLiteral b $ simplify <$> es
simplify (ETupleLiteral b es)  = ETupleLiteral b $ simplify <$> es
simplify e                     = e

-- | 先前parsing的过程将所有二元运算符看做了左结合,优先级5
-- 在这里把二元运算的结合顺序恢复为正确的顺序
balance :: [InfixDef] -> ExpressionX a -> ExpressionX a
balance ids = simplify . balance' (translateDB ids)
  where translateDB                          = fmap translate
        translate (InfixDef name prec assoc) = (name, (assoc, prec))

