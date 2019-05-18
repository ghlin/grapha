module Lang.Literal where

data Literal
  = LInteger   Int
  | LDouble    Double -- TODO
  | LString    String
  | LChar      Char
  deriving (Show, Eq)

