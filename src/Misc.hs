module Misc where
import           Control.Monad                  ( guard )

type Name = String

singleton :: a -> [a]
singleton = (:[])

tupleCon :: Int -> String
tupleCon n = "(" ++ replicate (n - 1) ',' ++ ")"

split :: (a -> Bool) -> [a] -> ([a], [a])
split p = s [] []
  where
    s y n [] = (reverse y, reverse n)
    s y n (k : ks) | p k       = s (k : y) n ks
                   | otherwise = s y (k : n) ks

topo :: Ord k => [((k, [k]), a)] -> Maybe [((k, [k]), a)]
topo = t [] []
  where satisfied s ((_, deps), _) = all (`elem` s) deps
        names = fmap $ fst . fst
        t _ res [] = return res
        t s res ks = do let (ok, nope) = split (satisfied s) ks
                        guard $ not $ null ok
                        t (s <> names ok) (res <> ok) nope
