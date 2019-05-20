module Pipes.PreludeInjector
  ( injectPrelude
  ) where


import           Lang.Surface
import           Lang.Type
import           Pipe
import           Misc

injectPrelude :: Pipe ErrorMessage Program Program
injectPrelude prog = let dts = dataTypeDefs prog
                      in return $ prog { dataTypeDefs = preludeDatatypes <> dts }

mkTupleDef :: Int -> DataTypeDef
mkTupleDef arity = let name = tupleCon arity
                       vars = take arity enumIds
                       con  = ProductDef name $ TVar <$> vars
                    in DataTypeDef name vars [con]

preludeDatatypes :: [DataTypeDef]
preludeDatatypes = [list, bool, int, string] <> tuples
  where
    list   = DataTypeDef "[]" ["a"] [nil, cons]
    nil    = ProductDef  "[]" []
    cons   = ProductDef  "::" [TVar "a", TApp (TCon "[]") (TVar "a")]

    bool   = DataTypeDef "Bool"  []    [true, false]
    true   = ProductDef  "True"  []
    false  = ProductDef  "False" []

    int    = DataTypeDef "Int"    [] []
    string = DataTypeDef "String" [] []

    tuples = mkTupleDef <$> [2 .. 42]

