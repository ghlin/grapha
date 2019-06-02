{-# LANGUAGE OverloadedStrings #-}
module Lang.SC
  ( SC     (..)
  , SCExpr (..)
  , SCProgram
  , prettySC
  , prettySCExpr
  ) where

import           Data.Text.Prettyprint.Doc
import           Text.Printf                    ( printf )
import           Lang.Literal
import           Misc

data SC
  = SC Name [Name] SCExpr
  deriving (Show)

data SCExpr
  = SCLit     Literal
  | SCVar     Name
  | SCApp     SCExpr SCExpr
  | SCBuiltin Name Int -- ^ name & arity
  | SCPack    Name Int -- ^ constructor & arity
  | SCPick    Int      -- ^ which component to pick
  | SCTest    Name     -- ^ test if of a given constructor
  | SCIf      SCExpr SCExpr SCExpr
  | SCLet     Name SCExpr SCExpr
  | SCLetRec  [(Name, SCExpr)] SCExpr -- ^ mutal rec let
  deriving (Show)

type SCProgram = ([SC], Name)    -- ^ sequence of sc defns, and the entry sc name 

flattenSC :: SCExpr -> [SCExpr]
flattenSC = reverse . f
  where f (SCApp e1 e2) = e2:f e1
        f e             = [e]

parensIf :: Bool -> Doc a -> Doc a
parensIf c = if c then parens else id

atom :: SCExpr -> Bool
atom SCApp{}    = False
atom SCIf{}     = False
atom SCLet{}    = False
atom SCLetRec{} = False
atom _          = True

laidout :: SCExpr -> Bool
laidout SCIf{}        = True
laidout SCLet{}       = True
laidout SCLetRec{}    = True
laidout (SCApp e1 e2) = laidout e1 || laidout e2
laidout _             = False

app :: SCExpr -> Bool
app SCApp{} = True
app _       = False

parensExp :: SCExpr -> Doc a
parensExp e = parensIf (not $ atom e) $ pretty e

parensApp :: SCExpr -> Doc a
parensApp e = parensIf (app e) $ pretty e

valign :: [Doc a] -> Doc a
valign = align . vcat

hspcat :: [Doc a] -> Doc a
hspcat = concatWith (surround space)

instance Pretty SC where
  pretty (SC name args body) = pretty name <+> hspcat (pretty <$> args) <+> "=" <+> pretty body

instance Pretty SCExpr where
  pretty (SCLit l) = pretty l
  pretty (SCVar v) = pretty v
  pretty (SCIf c t e) =
    "IF" <+> valign [pretty c, "THEN" <+> pretty t, "ELSE" <+> pretty e]
  pretty (SCLet v b e) =
    valign ["LET" <+> align (pretty v <+> "=" <+> pretty b), "IN " <+> pretty e]
  pretty (SCLetRec bindings e) =
    valign ["LET REC" <+> valign (ppBinding <$> bindings), "IN     " <+> pretty e]
      where ppBinding (v, b) = pretty v <+> "=" <+> pretty b
  pretty (SCBuiltin v _) = pretty v
  pretty (SCPack n a) = pretty $ (printf "pack{%s,%d}" n a :: String)
  pretty (SCPick n  ) = pretty $ (printf "pick{%d}" n :: String)
  pretty (SCTest n  ) = pretty $ (printf "test{%s}" n :: String)
  pretty (e@SCApp{} ) = prettyApps $ flattenSC e
    where prettyApps (a:as)
            | any laidout as = parensIf (laidout a)    (pretty a) <+> valign (parensApp <$> as)
            | otherwise      = parensIf (not $ atom a) (pretty a) <+> hspcat (parensExp <$> as)

prettySC :: SC -> String
prettySC = show . pretty

prettySCExpr :: SCExpr -> String
prettySCExpr = show . pretty
