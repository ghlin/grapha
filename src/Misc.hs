module Misc where

type Name = String

singleton :: a -> [a]
singleton = (:[])
