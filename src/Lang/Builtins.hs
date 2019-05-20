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
  , ("getline",   0,   tString)
  , ("putstr",    2,   tString `fn` tUnit)
  , ("error",     1,   tString `fn` TVar "a")
  , ("undefined", 0,   TVar "a")
  ]
