module Lang.Builtins
  ( missingCaseVarName
  , builtinCombinatorSignatures
  , BuiltinCombinatorSignature
  ) where

import           Lang.Type
import           Misc

missingCaseVarName :: String
missingCaseVarName = "undefined"

type BuiltinCombinatorSignature = (Name, Int, Type)

builtinCombinatorSignatures :: [BuiltinCombinatorSignature]
builtinCombinatorSignatures =
  [ ("==",        2,   tInt `fn` (tInt `fn` tBool))
  , (">",         2,   tInt `fn` (tInt `fn` tBool))
  , ("<",         2,   tInt `fn` (tInt `fn` tBool))
  , (">=",        2,   tInt `fn` (tInt `fn` tBool))
  , ("<=",        2,   tInt `fn` (tInt `fn` tBool))
  , ("!=",        2,   tInt `fn` (tInt `fn` tBool))
  , ("+",         2,   tInt `fn` (tInt `fn` tInt))
  , ("-",         2,   tInt `fn` (tInt `fn` tInt))
  , ("*",         2,   tInt `fn` (tInt `fn` tInt))
  , ("/",         2,   tInt `fn` (tInt `fn` tInt))
  , ("get-char",  0,   tChar)
  , ("get-int",   0,   tInt)
  , ("put-char",  1,   tChar `fn` tInt)
  , ("put-int",   1,   tInt  `fn` tInt)
  , ("seq",       2,   b `fn` (a `fn` a))
  , ("undefined", 0,   a)
  ]
    where a = TVar "a"
          b = TVar "b"
