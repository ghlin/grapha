module Lang.Literal where

import           Data.Text.Prettyprint.Doc

data Literal
  = LInteger   Int
  | LDouble    Double -- TODO
  | LString    String
  | LChar      Char
  deriving (Show, Eq)

instance Pretty Literal where
  pretty (LInteger v) = pretty v
  pretty (LDouble  d) = pretty d
  pretty (LChar    c) = pretty ['\'', c, '\'']
  pretty (LString  s) = pretty s
