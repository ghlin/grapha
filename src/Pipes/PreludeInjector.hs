{-# LANGUAGE QuasiQuotes #-}

module Pipes.PreludeInjector
  ( injectPrelude
  ) where


import           Text.RawString.QQ              ( r )
import           Lang.Surface
import           Lang.Type
import           Pipes.Parser
import           Pipe
import           Misc

injectPrelude :: Pipe ErrorMessage Program Program
injectPrelude prog = do Program preludeC _ preludeI <- parse $ Source "<prelude>" preludeSource
                        return $ prog { dataTypeDefs   = preludeDatatypes <> dataTypeDefs   prog
                                      , combinatorDefs = preludeC         <> combinatorDefs prog
                                      , infixDefs      = preludeI         <> infixDefs      prog
                                      }

mkTupleDef :: Int -> DataTypeDef
mkTupleDef arity = let name = tupleCon arity
                       vars = take arity enumIds
                       con  = ProductDef name $ TVar <$> vars
                    in DataTypeDef name vars [con]

preludeDatatypes :: [DataTypeDef]
preludeDatatypes = [list, bool, int, string, char, unit] <> tuples
  where
    list   = DataTypeDef "[]" ["a"] [nil, cons]
    nil    = ProductDef  "[]" []
    cons   = ProductDef  "::" [TVar "a", TApp (TCon "[]") (TVar "a")]

    bool   = DataTypeDef "Bool"  []    [true, false]
    true   = ProductDef  "True"  []
    false  = ProductDef  "False" []

    int    = DataTypeDef "Int"    [] []
    string = DataTypeDef "String" [] []
    char   = DataTypeDef "Char"   [] []

    unit   = mkTupleDef 0

    tuples = mkTupleDef <$> [2 .. 42]

preludeSource :: String
preludeSource = [r|

id x = x

infixl 3 ||
infixl 4 &&

False || False = False
*     || *     = True

True && True = True
*    && *    = False

foldl * acc []      = acc
foldl f acc (x::xs) = f (foldl f acc xs) x

foldr * acc []      = acc
foldr f acc (x::xs) = foldr f (f x acc) xs

infixr 0 $
f $ x = f x

infixr 5 ::

map * []      = []
map f (x::xs) = f x :: map f xs

filter * []      = []
filter p (x::xs) = if p x
  then x :: filter p xs
  else      filter p xs

fst (x, y) = x

snd (x, y) = y

head (x::xs) = x

tail []      = []
tail (x::xs) = []

concat []      ys = ys
concat (x::xs) ys = x::concat xs ys

reverse ins = let rev acc ys = case ys of
                                 []    -> acc
                                 x::xs -> rev (x::acc) ys
              in  rev [] ins

enum-from n = n::enum-from (n + 1)

drop * []      = []
drop 0 xs      = xs
drop n (x::xs) = drop (n - 1) xs

take * []      = []
take 0 *       = []
take n (x::xs) = x::take (n - 1) xs

zip-with * []      *       = []
zip-with * *       []      = []
zip-with f (x::xs) (y::ys) = f x y::zip-with f xs ys

zip []      *       = []
zip *       []      = []
zip (x::xs) (y::ys) = (x, y)::zip xs ys

null [] = True
null *  = False

(x::xs) !! 0 = x
(x::xs) !! n = xs !! (n - 1)

put-chars []      = ()
put-chars (x::xs) = seq (put-char x) (put-chars xs)

put-ints []      = ()
put-ints [x]     = seq (put-int x) ()
put-ints (x::xs) = seq (seq (put-int x) (put-char ',')) (put-ints xs)

|]
