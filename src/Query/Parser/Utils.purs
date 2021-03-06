module Query.Parser.Utils where

import Data.StrMap as SM
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List (List, many)
import Data.Maybe (Maybe, maybe)
import Data.Number (fromString)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.Combinators (between, try)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (class StringLike, char, oneOf, string, whiteSpace)
import Text.Parsing.Parser.Token (GenLanguageDef(LanguageDef), GenTokenParser, LanguageDef, alphaNum, digit, makeTokenParser, unGenLanguageDef)
import Prelude (class Monad, map, pure, ($), (*>), (<$>), (<*), (<*>), (<<<), (=<<))


parens :: forall m a. Monad m => ParserT String m a -> ParserT String m a
parens = between (string "(" *> whiteSpace) (whiteSpace <* string ")")

betweenLax :: forall m s a open close. Monad m => StringLike s => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
betweenLax before after = between (lax before) (lax after)

betweenOptional :: forall m s a open close. Monad m => StringLike s => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
betweenOptional before after p = betweenLax before after p <|> p

lax :: forall a m s. Monad m => StringLike s => ParserT s m a -> ParserT s m a
lax p = whiteSpace *> p <* whiteSpace

tuple :: forall a b. ParserT String Identity a -> ParserT String Identity b -> ParserT String Identity (Tuple a b)
tuple a b = queryParser.parens $ Tuple <$> (a <* lax queryParser.comma) <*> b

-- Parses '3_hello-24: 3736'
propTuple :: forall a b m s. Monad m => StringLike s => ParserT s m a -> ParserT s m b -> ParserT s m (Tuple a b)
propTuple a b = Tuple <$> (a <* lax (string ":")) <*> b

list :: forall a. ParserT String Identity a -> ParserT String Identity (List a)
list p = queryParser.brackets ps where
  ps = queryParser.commaSep p

listFlex :: forall a. ParserT String Identity a -> ParserT String Identity (List a)
listFlex p = queryParser.brackets ps <|> ps where
  ps = queryParser.commaSep p
   
strMap :: forall a. ParserT String Identity a -> ParserT String Identity (SM.StrMap a)
strMap = map SM.fromFoldable <<< listFlex <<< propTuple queryParser.identifier

emptyStrMap :: forall a. a -> ParserT String Identity a -> ParserT String Identity (SM.StrMap a)
emptyStrMap def p = map SM.fromFoldable $ listFlex $ (try $ propTuple queryParser.identifier p) <|> (map toTuple queryParser.identifier)
  where toTuple x = Tuple x def

number :: ∀ m. Monad m ⇒ ParserT String m Number
number = (go <<< toEither <<< fromString <<< fromCharArray <<< fromFoldable) =<< (many digit)
  where
    toEither :: forall b. Maybe b -> Either String b
    toEither = maybe (Left "Error parsing String to Number") Right

    go (Left e) = fail e
    go (Right v) = pure v

queryParser :: GenTokenParser String Identity
queryParser = makeTokenParser javaStyle where
  javaStyle  :: LanguageDef
  javaStyle   = LanguageDef (unGenLanguageDef emptyDef)
                { commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = "//"
                , nestedComments  = false
                , identStart      = alphaNum <|> char '_'
                , identLetter     = alphaNum <|> oneOf ['_', '-']
                , reservedNames   = []
                , reservedOpNames = []
                , caseSensitive   = false
                }
