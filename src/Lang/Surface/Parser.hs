{-# LANGUAGE TupleSections #-}
module Lang.Surface.Parser
where

import           Control.Monad                  ( void, when, (>=>) )
import           Control.Monad.Combinators.Expr
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( isJust )
import           Text.Megaparsec
import           Text.Megaparsec.Char    hiding ( string )
import qualified Text.Megaparsec.Char.Lexer    as L

import           Lang.Surface
import           ParserHelper                  as H
import           Misc                           ( singleton, Name )

-- {{{

parser :: P Program
parser = organize <$> (between scn eof $ many $ lexemeN toplevel)
  where organize = foldl pick prog
        prog = Program [] [] [] [] [] [] []
        pick p (ToplevelInterfaceDef    d) = p { interfaceDefs    = d:interfaceDefs    p }
        pick p (ToplevelInstanceDef     d) = p { instanceDefs     = d:instanceDefs     p }
        pick p (ToplevelCombinatorDef   d) = p { combinatorDefs   = d:combinatorDefs   p }
        pick p (ToplevelCombinatorAnnot d) = p { combinatorAnnots = d:combinatorAnnots p }
        pick p (ToplevelAliasDef        d) = p { aliasDefs        = d:aliasDefs        p }
        pick p (ToplevelDataTypeDef     d) = p { dataTypeDefs     = d:dataTypeDefs     p }
        pick p (ToplevelInfixDef        d) = p { infixDefs        = d:infixDefs        p }

-- }}}

-- {{{ literal parsers

-- | parse a literal
-- TODO: LDouble & LChar
literal :: P Literal
literal = LString  <$> lexeme H.string
      <|> LInteger <$> lexeme H.integer

-- }}}

-- {{{ type / type scheme parsers

-- a -> (b -> c)
-- f a -> f b -> [a] -> (a, b)

-- | type? typing!
typing :: P Type
typing = foldl1 fn <$> termT `sepBy1` symbol "->"

fn :: Type -> Type -> Type
fn t1 t2 = TApp () (TApp () (TCon () "->") t1) t2

varT :: P Type
varT = TVar () <$> identV

conT :: P Type
conT = TCon () <$> identT

tupleCon :: Int -> String
tupleCon n = "(" ++ replicate (n - 1) ',' ++ ")"

tupleT :: P Type
tupleT = do
  ts <- parens $ typing `sepBy` comma
  case ts of
    []  -> return $ TCon () "()"
    [_] -> fail "quote"
    _   -> return $ foldl1 (TApp ()) $ (TCon () $ tupleCon $ length ts):ts

listT :: P Type
listT = TApp () (TCon () "[]") <$> brackets typing

termT :: P Type
termT = listT <|> ((varT <|> conT) >>= probe) <|> try tupleT <|> parens typing
  where probe t1 = fallback' t1 $ TApp () t1 <$> termT


-- | Qual
qual :: P (Qual Type)
qual = Qual <$> fallback' [] (predsQ <* fatArrowR) <*> typing

predi :: P Pred
predi = Pred <$> identT <*> typing

predsQ :: P [Pred]
predsQ = braces (predi `sepBy1` comma) <|> singleton <$> predi

-- }}}

-- {{{ pattern

-- | pattern!
pattern :: P Pattern
pattern = makeExprParser termP [[infix']]
  where infix' = InfixL $ do
          con <- identC
          return $ \a b -> PCon con [a, b]

varP :: P Pattern
varP = PVar <$> identV

litP :: P Pattern
litP = PLit <$> literal

wildcardP :: P Pattern
wildcardP = PWildcard <$ asterisk

tupleP :: P Pattern
tupleP = do
  ps <- parens $ pattern `sepBy` comma
  let arity = length ps
  when (arity == 1) $ fail "quotation" -- 此处会引起回溯
                                       -- 可以在arity=1时构造单个pattern
  return $ PCon (tupleCon arity) ps

listP :: P Pattern
listP = PCon "[]" <$> brackets (pattern `sepBy` comma)

conP :: P Pattern
conP = PCon <$> identT <*> many pattern

termP :: P Pattern
termP = litP <|> varP <|> conP <|> wildcardP <|> listP <|> try tupleP <|> parens pattern

-- | `of type` assertion
ofTypeAssertion :: P Scheme
ofTypeAssertion = colon >> Forall [] <$> qual

-- | `<symbol> : <type>` assertion
annotation :: P Annotation
annotation = Annotation <$> identV <*> ofTypeAssertion

-- }}}

-- {{{ expression....

-- | expression parser
expr :: P Expression
expr = ifE <|> doE <|> try letE <|> caseE <|> lamE <|> expr'

litE :: P Expression
litE = ELit Nothing <$> literal

varE :: P Expression
varE = EVar Nothing <$> (identV <|> identT)

lamE :: P Expression
lamE = ELam Nothing <$> (backslash *> some pattern)
                    <*> (arrowR    *> expr)

ifE :: P Expression
ifE = do
  c <- symbol "if" *> expr
  (t, e) <- try compact <|> laidout
  return $ EIf Nothing c t e
  where compact = (,) <$> (symbol "then" *> expr) <*> (symbol "else" *> expr)
        laidout = aligned $ \ref -> do
          c <- indented ref $ symbol "then" *> expr <* scn
          e <- indented ref $ symbol "else" *> expr
          return (c, e)

caseE :: P Expression
caseE = do
  exam <- symbol "case" *> expr <* symbol "of"
  alts <- try compact <|> laidout
  return $ ECase Nothing exam alts
  where compact = caseAltE `sepBy1` semi
        laidout = someAligned caseAltE

caseAltE :: P CaseAlternative
caseAltE = CaseAlternative <$> pattern <*> (arrowR *> expr)

letE :: P Expression
letE = aligned $ \ref -> do
  indented ref $ symbol "let"
  bindings <- someAligned letBindingB
  indented ref $ symbol "in"
  body <- expr
  return $ ELet bindings body

letBindingB :: P LetBinding
letBindingB = try letBindingB' <|> letAnnotationB

letAnnotationB :: P LetBinding
letAnnotationB = BindingAnnotation <$> annotation

letBindingB' :: P LetBinding
letBindingB' = LetBinding <$> letBindingFormB <*> (equalsign *> expr)

letBindingFormB :: P LetBindingForm
letBindingFormB = try (PatternBinding <$> pattern <* lookAhead (equalsign)) <|> combinatorBindingFormB

combinatorBindingFormB :: P LetBindingForm
combinatorBindingFormB = CombinatorBinding <$> identV <*> many pattern

doE :: P Expression
doE = EDo Nothing <$> (symbol "do" *> someAligned doStmtS)

doLetS :: P DoStmt
doLetS = symbol "let" *> (DoLetBinding <$> letBindingFormB <*> (equalsign *> expr))

doBindS :: P DoStmt
doBindS = optional (try $ pattern <* arrowL) >>= probe
  where probe mpat = DoBind mpat <$> expr

doStmtS :: P DoStmt
doStmtS = doLetS <|> doBindS

listLiteralE :: P Expression
listLiteralE = EListLiteral Nothing <$> brackets (expr `sepBy` comma)

tupleLiteralE :: P Expression
tupleLiteralE = do
  es <- parens $ expr `sepBy` comma
  when (length es == 1) $ fail "quotation"
  return $ ETupleLiteral Nothing es

termE :: P Expression
termE = termE' >>= probe
  where
    probe = probeApp probe >=> probeAnnot probe
    probeApp p e1 = fallback' e1 $ do
      e2 <- termE'
      p $ EApp Nothing e1 e2
    probeAnnot p e = fallback' e $ do
      annot <- ofTypeAssertion
      p $ putAnnot annot e

termE' :: P Expression
termE' = litE
     <|> try varE
     <|> try tupleLiteralE
     <|> listLiteralE
     <|> EQuoted Nothing <$> parens expr -- TODO: prefix operator

infixOpE :: P Name
infixOpE = identO <|> infixFnE

infixFnE :: P Name
infixFnE = backquotes $ identV <|> identO

expr' :: P Expression
expr' = do
  e1 <- termE
  fallback' e1 $ do
    o1 <- infixOpE
    rs <- many $ try $ (,) <$> termE <*> infixOpE
    el <- expr
    return $ rotate $ build el $ (e1, o1):rs
      where build el ps                              = foldr mk el ps
            mk (l, o) r                              = EBinary Nothing o l r
            rotate (EBinary _ α a (EBinary _ β b c)) = rotate $ EBinary Nothing β (EBinary Nothing α a b) c
            rotate x                                 = x

-- }}}

-- {{{ toplevel defs

-- | toplevel
toplevel :: P ToplevelDef
toplevel = (ToplevelInfixDef        <$> infixDef)
       <|> (ToplevelAliasDef        <$> aliasDef)
       <|> (ToplevelDataTypeDef     <$> dataTypeDef)
       <|> (ToplevelInterfaceDef    <$> (lookAhead (symbol "interface") *> interfaceDef))
       <|> (ToplevelInstanceDef     <$> instanceDef)
       <|> (ToplevelCombinatorAnnot <$> try combinatorAnnot)
       <|> (ToplevelCombinatorDef   <$> combinatorDef)

-- | alias: alias P a = Parsec ... a
aliasDef :: P AliasDef
aliasDef = AliasDef <$> (symbol "alias" *> identT)
                    <*> argsD
                    <*> (equalsign *> typing)

-- | infix[l|r] <prece> <op>
-- e.g. infixl 4 (<$>)
infixDef :: P InfixDef
infixDef = do
  assoc <- assocI
  prec  <- integer
  name  <- parens  (identC <|> identO) <|> infixFnE
  return $ InfixDef name prec assoc

assocI :: P InfixAssoc
assocI = try (symbol "infixl" $> AssocLeft)
     <|> try (symbol "infixr" $> AssocRight)
     <|>     (symbol "infix"  $> AssocNone)


combinatorNameC :: P Name
combinatorNameC = parens identO <|> identV

-- | combinator def
-- f x y = ...
-- (<+>) x y = ...
combinatorDef :: P CombinatorDef
combinatorDef = CombinatorDef <$> combinatorNameC
                              <*> (many pattern <* equalsign)
                              <*> expr

argsD :: P [(Name, ())]
argsD = fmap (,()) <$> many identV

-- | combinator annotation
-- main : IO ()
combinatorAnnot :: P CombinatorAnnot
combinatorAnnot = CombinatorAnnot <$> combinatorNameC
                                  <*> ofTypeAssertion

-- | data type
dataTypeDef :: P DataTypeDef
dataTypeDef = do
  symbol "datatype"
  name  <- identT
  args  <- argsD
  prods <- try compact <|> laidout
  return $ DataTypeDef name args prods
  where compact = equalsign *> productD `sepBy` vertbar
        laidout = aligned $ \ref -> do
          indented ref equalsign
          productD `sepBy1` try (optional scn *> indented ref vertbar)

productD :: P ProductDef
productD = ProductDef <$> identT <*> many termT

-- | interface [ <preds> => ] <name> <arg>
--     <member-defs>
interfaceDef :: P InterfaceDef
interfaceDef = do
  symbol "interface"
  (preds, name, arg) <- try full <|> simple
  members <- aligned $ \ref ->
    combinatorAnnot `sepBy1` try (scn *> indented ref (return ()))
  return $ InterfaceDef name preds (arg, ()) members
  where full = (,,) <$> predsQ <* fatArrowR <*> identT <*> identV
        simple = ([],,) <$> identT <*> identV

-- | instance [ <preds> => ] <name> <type>
instanceDef :: P InstanceDef
instanceDef = do
  symbol "instance"
  (preds, name, ty) <- try full <|> simple
  members <- aligned $ \ref ->
    combinatorDef `sepBy1` try (scn *> indented ref (return ()))
  return $ InstanceDef name preds ty members
  where full   = (,,) <$> predsQ <* fatArrowR <*> identT <*> typing
        simple = ([],,) <$> identT <*> typing

-- }}}

-- {{{ misc

arrowR, arrowL, fatArrowR :: P String
arrowR    = symbol "->"
arrowL    = symbol "<-"
fatArrowR = symbol "=>"

upperChars, letterChars, digitChars :: String
letterChars = ['a' .. 'z']
upperChars  = ['A' .. 'Z']
digitChars  = ['0' .. '9']

op1Chars, opChars :: String
op1Chars  = "<+-:~/%&?!.>*=@$"
opChars   = op1Chars ++ "|#"

var1Chars, varChars, con1Chars, conChars :: String
var1Chars = letterChars
varChars  = letterChars ++ upperChars ++ digitChars ++ "'-_"
con1Chars = upperChars
conChars  = varChars

identT :: P String
identT = identOf con1Chars conChars []

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
       , "alias"
       , "instance"
       , "interface"
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
       ]

-- }}}
