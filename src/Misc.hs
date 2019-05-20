module Misc
  ( Name
  , ErrorMessage
  , isConstr
  , singleton
  , tupleCon
  , split
  , topo
  , topo'
  , enumIds
  ) where

import           Control.Monad                  ( guard )
import           Data.Char

type Name = String

type ErrorMessage = String

enumIds :: [Name]
enumIds = do part <- "":enumIds
             pend <- ['a' .. 'z']
             return $ reverse $ pend:part


singleton :: a -> [a]
singleton = (:[])

tupleCon :: Int -> String
tupleCon 0 = "()"
tupleCon n = "(" ++ replicate (n - 1) ',' ++ ")"

split :: (a -> Bool) -> [a] -> ([a], [a])
split p = s [] []
  where
    s y n [] = (reverse y, reverse n)
    s y n (k : ks) | p k       = s (k : y) n ks
                   | otherwise = s y (k : n) ks

type G k a = ((k, [k]), a)

topo :: Ord k => [G k a] -> Maybe [G k a]
topo ks = case topo' ks of
            (good, []) -> Just good
            _          -> Nothing

topo' :: Ord k => [G k a] -> ([G k a], [G k a])
topo' = t [] []
  where satisfied s ((k, deps), _) = all (`elem` (k:s)) deps
        names = fmap $ fst . fst
        t _ res [] = (res, [])
        t s res ks = let (ok, nope) = split (satisfied s) ks
                      in if null ok
                            then (res, nope)
                            else t (s <> names ok) (res <> ok) nope

isConstr :: Name -> Bool
isConstr "[]" = True
isConstr n    = isUpper (head n) || (head n == ':') || (head n == '(')

