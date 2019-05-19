module Pipe where

type Pipe e a b = a -> Either e b
