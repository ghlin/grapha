module Lang.Simpl.Translator where

import           Control.Monad.Trans.State     as T
import Control.Monad (mapM, replicateM, foldM)
import           Lang.Surface
import           Lang.Surface.Subst
import           Lang.Simpl                    as S
import           Misc

data TState
  = TState
    { supply :: Int
    , envs   :: ()
    }
    deriving (Show, Eq)

type M a = T.State TState a

acquireId :: M Name
acquireId = do
  i <- (+ 1) <$> T.gets supply
  T.modify $ \s -> s { supply = i }
  return $ "u{" <> show i <> "}"

translateE :: Expression -> M SimplExpr
translateE = undefined

