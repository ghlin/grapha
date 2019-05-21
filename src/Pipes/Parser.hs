{-# LANGUAGE TupleSections #-}

module Pipes.Parser
  ( parse
  , Source (..)
  ) where

import           Control.Monad                  ( void
                                                , when
                                                , (>=>)
                                                )
import           Control.Monad.Combinators.Expr
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( isJust )
import           Text.Megaparsec         hiding ( parse )
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           Text.Megaparsec.Char    hiding ( string )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Lang.Surface
import           Lang.Literal
import           Lang.Type                     as T
import           ParserHelper                  as H
import           Lang.Builtins
import           Misc
import           Pipe

-- {{{

data Source
  = Source
    { sourceFileName    :: String
    , sourceFileContent :: String
    }

parse :: Pipe ErrorMessage Source Program
parse (Source name src) =
  case runParser parser name src of
    Left e  -> Left $ errorBundlePretty e
    Right p -> return p

parser :: P Program
parser = organize <$> between scn eof (many $ lexemeN toplevel)
  where organize = foldl pick prog
        prog = Program [] [] []
        pick p (ToplevelCombinatorDef   d) = p { combinatorDefs   = combinatorDefs   p <> [d] }
        pick p (ToplevelDataTypeDef     d) = p { dataTypeDefs     = dataTypeDefs     p <> [d] }
        pick p (ToplevelInfixDef        d) = p { infixDefs        = infixDefs        p <> [d] }
-- }}}

-- {{{ literal parsers

-- | parse a literal
-- TODO: LDouble & LChar
literal :: P Literal
literal = LString  <$> lexeme H.string
      <|> LInteger <$> lexeme H.integer
      <|> LChar    <$> lexeme H.character
-- }}}

-- {{{ type / type scheme parsers

-- a -> (b -> c)
-- f a -> f b -> [a] -> (a, b)

-- | type? typing!
typing :: P Type
typing = foldl1 T.fn <$> termT `sepBy1` arrowR

-- | like typing, but doesn't probe type application
typing' :: P Type
typing' = foldl1 T.fn <$> termT' `sepBy1` arrowR

varT :: P Type
varT = TVar <$> identV

conT :: P Type
conT = TCon <$> identT

tupleT :: P Type
tupleT = do
  ts <- parens $ typing `sepBy` comma
  case ts of
    []  -> return $ TCon "()"
    [_] -> fail "quote"
    _   -> return $ foldl1 TApp $ (TCon $ tupleCon $ length ts):ts

listT :: P Type
listT = TApp (TCon "[]") <$> brackets typing

termT :: P Type
termT = listT <|> ((varT <|> conT) >>= probe) <|> try tupleT <|> (parens typing >>= probe)
  where probe t1 = fallback' t1 $ (TApp t1 <$> termT') >>= probe

termT' :: P Type
termT' = listT <|> (varT <|> conT) <|> try tupleT <|> parens typing

-- }}}

-- {{{ pattern

pattern' :: P Pattern
pattern' = termP'

-- | pattern! (名字`pattern`会导致hlint抱怨'Parse error...' https://github.com/ndmitchell/hlint/issues/236)
pattern_ :: P Pattern
pattern_ = makeExprParser termP [[infix']]
  where infix' = InfixL $ do
          con <- infixT
          return $ \a b -> PCon con [a, b]

varP :: P Pattern
varP = PVar <$> identV

litP :: P Pattern
litP = PLit <$> literal

wildcardP :: P Pattern
wildcardP = PWildcard <$ asterisk

tupleP :: P Pattern
tupleP = do
  ps <- parens $ pattern_ `sepBy` comma
  let arity = length ps
  when (arity == 1) $ fail "quotation" -- 此处会引起回溯
                                       -- 可以在arity=1时构造单个pattern_
  return $ PCon (tupleCon arity) ps

listP :: P Pattern
listP = PCon "[]" <$> brackets (pattern_ `sepBy` comma)

conP :: P Pattern
conP = PCon <$> identT <*> many pattern'

conP' :: P Pattern
conP' = flip PCon [] <$> identT

termP :: P Pattern
termP = litP <|> varP <|> conP <|> wildcardP <|> listP <|> try tupleP <|> parens pattern_

termP' :: P Pattern
termP' = litP <|> varP <|> conP' <|> wildcardP <|> listP <|> try tupleP <|> parens pattern_

-- }}}

-- {{{ expression....

-- | expression parser
expr :: P Expression
expr = ifE <|> try letE <|> caseE <|> lamE <|> expr'

litE :: P Expression
litE = ELit <$> literal

varE :: P Expression
varE = EVar <$> (identV <|> identT)

lamE :: P Expression
lamE = ELam <$> (backslash *> some pattern_)
            <*> (arrowR    *> expr)

ifE :: P Expression
ifE = do
  c <- symbol "if" *> expr
  (t, e) <- try compact <|> laidout
  return $ EIf c t e
  where compact = (,) <$> (symbol "then" *> expr) <*> (symbol "else" *> expr)
        laidout = aligned $ \ref -> do
          c <- indented ref $ symbol "then" *> expr <* scn
          e <- indented ref $ symbol "else" *> expr
          return (c, e)

caseE :: P Expression
caseE = do
  exam <- symbol "case" *> expr <* symbol "of"
  alts <- try compact <|> laidout
  return $ ECase exam alts
  where compact = caseAltE `sepBy1` semi
        laidout = someAligned caseAltE

caseAltE :: P CaseAlternative
caseAltE = CaseAlternative <$> pattern_ <*> (arrowR *> expr)

letE :: P Expression
letE = aligned $ \ref -> do
  indented ref $ symbol "let"
  bindings <- someAligned letBindingB
  indented ref $ symbol "in"
  body <- expr
  return $ ELet bindings body

letBindingB :: P LetBinding
letBindingB = LetBinding <$> letBindingFormB <*> (equalsign *> expr)

letBindingFormB :: P LetBindingForm
letBindingFormB = try (PatternBinding <$> pattern_ <* lookAhead equalsign) <|> combinatorBindingFormB

combinatorBindingFormB :: P LetBindingForm
combinatorBindingFormB = do
  (name, pats) <- combinatorLHS
  return $ CombinatorBinding name pats

listLiteralE :: P Expression
listLiteralE = EListLiteral <$> brackets (expr `sepBy` comma)

tupleLiteralE :: P Expression
tupleLiteralE = do
  es <- parens $ expr `sepBy` comma
  when (length es == 1) $ fail "quotation"
  return $ ETupleLiteral es

termE :: P Expression
termE = termE' >>= probeApp
  where
    probeApp e1 = fallback' e1 $ do
      e2 <- termE'
      probeApp $ EApp e1 e2

termE' :: P Expression
termE' = litE
     <|> try varE
     <|> try tupleLiteralE
     <|> listLiteralE
     <|> EQuoted <$> parens expr -- TODO: prefix operator

expr' :: P Expression
expr' = do
  e1 <- termE
  fallback' e1 $ do
    o1 <- lexemeN $ infixV <|> infixT
    rs <- many $ try $ (,) <$> termE <*> lexemeN (infixV <|> infixT)
    el <- expr
    return $ rotate $ build el $ (e1, o1):rs
      where build                                = foldr mk
            mk (l, o)                            = EBinary o l
            rotate (EBinary α a (EBinary β b c)) = rotate $ EBinary β (EBinary α a b) c
            rotate x                             = x

-- }}}

-- {{{ toplevel defs

-- | toplevel
toplevel :: P ToplevelDef
toplevel = (ToplevelInfixDef        <$> infixDef)
       <|> (ToplevelDataTypeDef     <$> dataTypeDef)
       <|> (ToplevelCombinatorDef   <$> combinatorDef)

-- | infix[l|r] <prece> <op>
-- e.g. infixl 4 <$>
infixDef :: P InfixDef
infixDef = do
  assoc <- assocI
  prec  <- integer
  name  <- infixV <|> infixT
  return $ InfixDef name prec assoc

assocI :: P InfixAssoc
assocI = try (symbol "infixl" $> AssocLeft)
     <|> try (symbol "infixr" $> AssocRight)
     <|> try (symbol "infix"  $> AssocNone) -- `try` is necessary, see `toplevel`

combinatorLHS :: P (Name, [Pattern])
combinatorLHS = try ((,) <$> combinatorNameC <*> many pattern' <* lookAhead equalsign) <|> do
  l <- pattern'
  c <- infixV
  r <- pattern'
  return (c, [l, r])

combinatorNameC :: P Name
combinatorNameC = parens identO <|> identV

-- | combinator def
-- f x y = ...
-- (<+>) x y = ...
combinatorDef :: P CombinatorDef
combinatorDef = do
  (name, args) <- combinatorLHS
  body <- equalsign *> expr
  return $ CombinatorDef name args body

-- | data type
dataTypeDef :: P DataTypeDef
dataTypeDef = do
  symbol "datatype"
  name  <- identT
  args  <- many identV
  prods <- try compact <|> laidout
  return $ DataTypeDef name args prods
  where compact = equalsign *> productD `sepBy` vertbar
        laidout = aligned $ \ref -> do
          indented ref equalsign
          productD `sepBy1` try (optional scn *> indented ref vertbar)

productD :: P ProductDef
productD = ProductDef <$> identT <*> many termT'

-- }}}

-- {{{ misc

arrowR, arrowL, fatArrowR :: P String
arrowR    = symbol "->"
arrowL    = symbol "<-"
fatArrowR = symbol "=>"

upperChars, lowerChars, digitChars :: String
lowerChars = ['a' .. 'z']
upperChars  = ['A' .. 'Z']
digitChars  = ['0' .. '9']

op1Chars, opChars :: String
op1Chars  = "<+-:~/%&?!.>*=@$|#"
opChars   = op1Chars ++ "|"

var1Chars, varChars, con1Chars, conChars :: String
var1Chars = lowerChars
varChars  = lowerChars ++ upperChars ++ digitChars ++ "'-_"
con1Chars = upperChars
conChars  = varChars

identT :: P String
identT = identOf con1Chars conChars []

infixT :: P String
infixT = backquotes identT <|> identC

infixV :: P String
infixV = backquotes identV <|> identO

identV :: P String
identV = identOf var1Chars varChars rwsV

identO :: P String
identO = identOf op1Chars opChars rwsO

identC :: P String
identC = identOf ":" opChars rwsO

rwsV :: [String]
rwsV = [ "let"
       , "in"
       , "if"
       , "then"
       , "else"
       , "case"
       , "of"
       , "with"
       , "type"
       , "where"
       , "infix"
       , "infixl"
       , "infixr"
       ]

rwsO :: [String]
rwsO = [ "->"
       , "=>"
       , "<-"
       , ":"
       , "|"
       , "="
       ]

-- }}}
