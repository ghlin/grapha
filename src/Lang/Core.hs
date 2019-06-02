{-# LANGUAGE OverloadedStrings #-}
module Lang.Core
  ( CoreCombinator (..)
  , CoreExpr       (..)
  , prettyCoreExpr
  , prettyCoreCombinator
  ) where

import           Data.Text.Prettyprint.Doc
import           Text.Printf                    ( printf )
import           Lang.Type
import           Lang.Literal
import           Misc

data CoreCombinator
  = CoreCombinator Name [Name] CoreExpr
  deriving (Show, Eq)

data CoreExpr
  = ELit  Literal
  | EVar  Name
  | ELam  [Name]   CoreExpr
  | EApp  CoreExpr CoreExpr
  | EIf   CoreExpr CoreExpr CoreExpr
  | ELet  [CoreCombinator]  CoreExpr
  | ETest Name     CoreExpr  -- ^ only the value constructor
  | EPick Name Int CoreExpr
  deriving (Show, Eq)

flattenCore :: CoreExpr -> [CoreExpr]
flattenCore = reverse . f
  where f (EApp e1 e2) = e2:f e1
        f e             = [e]

parensIf :: Bool -> Doc a -> Doc a
parensIf c = if c then parens else id

atom :: CoreExpr -> Bool
atom EApp{}  = False
atom ELet{}  = False
atom EIf{}   = False
atom ETest{} = False
atom EPick{} = False
atom _       = True

laidout :: CoreExpr -> Bool
laidout EIf{}         = True
laidout ELit{}        = True
laidout (EApp  l r  ) = laidout l || laidout r
laidout (ETest _ e  ) = laidout e
laidout (EPick _ _ e) = laidout e
laidout _             = False

app :: CoreExpr -> Bool
app EApp{} = True
app _      = False

parensExp :: CoreExpr -> Doc a
parensExp e = parensIf (not $ atom e) $ pretty e

parensApp :: CoreExpr -> Doc a
parensApp e = parensIf (app e) $ pretty e

valign :: [Doc a] -> Doc a
valign = align . vcat

hspcat :: [Doc a] -> Doc a
hspcat = concatWith (surround space)

instance Pretty CoreCombinator where
  pretty (CoreCombinator name args body) =
    pretty name <+> hspcat (pretty <$> args) <+> "=" <+> pretty body

instance Pretty CoreExpr where
  pretty (ELit l) = pretty l
  pretty (EVar n) = pretty n
  pretty (ELam args body) = "λ" <+> hspcat (pretty <$> args) <+> ". " <+> pretty body
  pretty (EIf c t e) =
    "if" <+> valign [pretty c, "then" <+> pretty t, "else" <+> pretty e]
  pretty (ELet bs body) =
    valign ["let" <+> valign (pretty <$> bs), "in " <+> pretty body]
  pretty (ETest con e  ) = pretty ("test{" <> con <> "}") <+> parensExp e
  pretty (EPick con f e) = pretty ("pick{" <> con <> "," <> show f <> "}") <+> parensExp e
  pretty e@EApp{} = prettyApps $ flattenCore e
    where prettyApps (a:as)
            | any laidout as = parensIf (laidout a)    (pretty a) <+> valign (parensApp <$> as)
            | otherwise      = parensIf (not $ atom a) (pretty a) <+> hspcat (parensExp <$> as)

prettyCoreCombinator :: CoreCombinator -> String
prettyCoreCombinator = show . pretty

prettyCoreExpr :: CoreExpr -> String
prettyCoreExpr = show . pretty
