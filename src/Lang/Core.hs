module Lang.Core where

import           Misc                           ( Name
                                                , tupleCon
                                                )
import           Lang.Kind
import           Lang.Type
import qualified Lang.Surface                  as S
import qualified Lang.Literal                  as S

data Pat
  = PLit Ty S.Literal
  | PVar Ty Name
  | PWildcard
  | PCon Ty [Pat]
  deriving (Show, Eq)

data Expr
  = ELit  Ty  S.Literal
  | EVar  Sc  Name
  | ELam  Sc  [Alt] Expr
  | EApp      Expr  Expr
  | EIf       Expr  Expr Expr
  | ELet      [Alt] Expr
  | ECase     Expr  [Alt]
  deriving (Show, Eq)

data Alt
  = Alt Name Sc Expr
  deriving (Show, Eq)



