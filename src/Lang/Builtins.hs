module Lang.Builtins
  ( missingCaseVarName
  , builtinCombinatorSignatures
  , BuiltinCombinatorSignature
  ) where

import           Lang.Type
import           Misc

missingCaseVarName :: String
missingCaseVarName = "undefined"

type BuiltinCombinatorSignature = (Name, Int, Type, [Bool])

allStrict :: [Bool]
allStrict = repeat True

builtinCombinatorSignatures :: [BuiltinCombinatorSignature]
builtinCombinatorSignatures =
  [ ("==",        2, a    `fn` (a    `fn` tBool), allStrict)
  , (">",         2, tInt `fn` (tInt `fn` tBool), allStrict)
  , ("<",         2, tInt `fn` (tInt `fn` tBool), allStrict)
  , (">=",        2, tInt `fn` (tInt `fn` tBool), allStrict)
  , ("<=",        2, tInt `fn` (tInt `fn` tBool), allStrict)
  , ("!=",        2, tInt `fn` (tInt `fn` tBool), allStrict)
  , ("+",         2, tInt `fn` (tInt `fn` tInt),  allStrict)
  , ("-",         2, tInt `fn` (tInt `fn` tInt),  allStrict)
  , ("*",         2, tInt `fn` (tInt `fn` tInt),  allStrict)
  , ("/",         2, tInt `fn` (tInt `fn` tInt),  allStrict)
  , ("%",         2, tInt `fn` (tInt `fn` tInt),  allStrict)
  , ("neg",       1, tInt,                        allStrict)
  , ("from-io",   1, a,                           allStrict)
  , ("get-char",  0, tChar,                       [])
  , ("get-int",   0, tInt,                        [])
  , ("put-char",  1, tChar `fn` tInt,             allStrict)
  , ("put-int",   1, tInt  `fn` tInt,             allStrict)
  , ("seq",       2, b `fn` (a `fn` a),           [True, False])
  , ("undefined", 0, a,                           [])
  ]
    where a = TVar "a"
          b = TVar "b"
