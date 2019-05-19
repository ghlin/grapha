module Pipes.ConstrCollector
  ( collectConstrs
  , Constr (..)
  , ConstrsTable
  ) where

import           Control.Monad
import           Lang.Surface            hiding ( fn )
import           Lang.Type
import           Pipe
import           Misc

data Constr
  = Constr
    { cArity      :: Int
    , cName       :: Name
    , cDataName   :: Name
    , cScheme     :: Scheme
    , cDataScheme :: Scheme
    , cComponents :: [Type]
    }
    deriving (Show)

type ConstrsTable = [(Name, Constr)]

collectConstrs :: Pipe ErrorMessage [DataTypeDef] ConstrsTable
collectConstrs = foldM collectOne []

insert :: Name -> ConstrsTable -> Pipe ErrorMessage Constr ConstrsTable
insert name cs constr = case lookup name cs of
                          Nothing -> Right $ (name, constr):cs
                          Just _  -> Left $ "Duplicated product name: " <> name <> ", of data type: " <> cDataName constr

mkScheme :: [Name] -> [Type] -> Type -> Scheme
mkScheme as fs ty = Forall as $ foldr fn ty fs

dataType :: Name -> [Name] -> Type
dataType name as = foldl TApp (TCon name) $ TVar <$> as

dataScheme :: Name -> [Name] -> Scheme
dataScheme name as = Forall as $ dataType name as

mkConstr :: DataTypeDef -> ProductDef -> Constr
mkConstr (DataTypeDef name as _) (ProductDef pname ts) =
  Constr { cArity      = length ts
         , cName       = pname
         , cDataName   = name
         , cScheme     = mkScheme as ts $ dataType name as
         , cDataScheme = dataScheme name as
         , cComponents = ts
         }

collectConstr :: DataTypeDef -> ProductDef -> Pipe ErrorMessage ConstrsTable ConstrsTable
collectConstr d p@(ProductDef name _) cs = insert name cs $ mkConstr d p

collectOne :: ConstrsTable -> Pipe ErrorMessage DataTypeDef ConstrsTable
collectOne cs d@(DataTypeDef _ _ ps) = foldM (flip $ collectConstr d) cs ps
