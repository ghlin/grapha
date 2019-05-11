module ParserHelper
  ( P
  , sc
  , scn
  , lexeme
  , symbol
  , lexemeN
  , symbolN
  , identOf
  , integer
  , string
  , surround
  , angles
  , braces
  , parens
  , singlequotes
  , doublequotes
  , backquotes
  , quotes
  , brackets
  , asterisk
  , colon
  , comma
  , vertbar
  , semi
  , backslash
  , equalsign
  , fallback
  , fallback'
  , aligned
  , aligned'
  , someAligned
  , someAligned'
  , manyAligned
  , manyAligned'
  , indented
  , testParser
  , linesOf
  , ($>)
  ) where

import           Data.Functor                   ( ($>) )
import           Data.Void                      ( Void )
import           Control.Arrow                  ( left )
import           Control.Monad                  ( void )
import           Text.Megaparsec
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           Text.Megaparsec.Char    hiding ( string )
import qualified Text.Megaparsec.Char.Lexer    as L

-- | The parser
type P = Parsec Void String

lc = L.skipLineComment  "--"
bc = L.skipBlockComment "{-" "-}"

sc :: P ()
sc = L.space cont lc bc
  where cont = void
          $ some
          $ char ' ' <|> char '\t' <|> try (char '\\' >> char '\n')

scn :: P ()
scn = L.space space1 lc bc

lexemeN :: P a -> P a
lexemeN = L.lexeme scn

symbolN :: String -> P String
symbolN = L.symbol scn

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: String -> P String
symbol = L.symbol sc

identOf :: [Char] -> [Char] -> [String] -> P String
identOf leading chars keywords = lexeme $ do
  sym <- (:) <$> oneOf leading <*> many (oneOf chars)
  if sym `elem` keywords
     then fail $ "Reserved: " <> sym
     else return sym

integer :: P Int
integer = lexeme L.decimal

string :: P String
string = char '"' *> manyTill L.charLiteral (char '"')

surround :: String -> String -> P a -> P a
surround l r = between (symbol l) (symbol r)

angles, braces, brackets, parens :: P a -> P a
angles   = surround "<" ">"
braces   = surround "{" "}"
brackets = surround "[" "]"
parens   = surround "(" ")"

quotes :: String -> P a -> P a
quotes c = surround c c -- FIXME: no lexeme

singlequotes, doublequotes, backquotes :: P a -> P a
singlequotes = quotes "'"
doublequotes = quotes "\""
backquotes   = quotes "`"

asterisk, comma, semi, colon, equalsign, backslash, vertbar :: P String
asterisk  = symbol "*"
colon     = symbol ":"
comma     = symbol ","
semi      = symbol ";"
vertbar   = symbol "|"
backslash = symbol "\\"
equalsign = symbol "="

-- reversed 'or else'
fallback :: P a -> P a -> P a
fallback a b = try b <|> a

fallback' :: a -> P a -> P a
fallback' = fallback . return

-- | indentation helper
indented :: Pos -> P a -> P a
indented ref p = do
  lvl <- scn *> L.indentLevel
  if ref == lvl
     then p
     else L.incorrectIndent EQ ref lvl

-- | probe the first indentation
aligned' :: P () -> (Pos -> P a) -> P a
aligned' s f = L.indentGuard s GT pos1 >>= f

-- | probe the first indentation, using scn
aligned :: (Pos -> P a) -> P a
aligned = aligned' scn

-- | aligned many
manyAligned' :: P () -> P a -> P [a]
manyAligned' s p = fallback' [] $ someAligned' s p

-- | aligned many, using scn
manyAligned :: P a -> P [a]
manyAligned = manyAligned' scn

-- | aligned some
someAligned' :: P () -> P a -> P [a]
someAligned' s p = aligned' s $ \pos -> do
  x  <- indented pos p
  xs <- many $ try $ indented pos p
  return $ x:xs

-- | aligned some, using scn
someAligned :: P a -> P [a]
someAligned = someAligned' scn

testParser :: P a -> String -> Either String a
testParser p = left errorBundlePretty . runParser p "<test>"

linesOf :: P a -> P [a]
linesOf p = between scn eol $ many (p <* eol)

