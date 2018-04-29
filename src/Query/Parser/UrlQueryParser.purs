module Query.Parser.UrlQueryParser where
import Query.Parser.Utils
import Prelude
import Query.Types
import Data.StrMap as SM
import Control.Alternative ((<|>))
import Control.Applicative ((<*))
import Data.Bifunctor (bimap)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.List (List)
import Data.Maybe (Maybe(Just, Nothing))
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..), uncurry)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators (notFollowedBy, try)
import Text.Parsing.Parser.String (eof, string)

-- -- Parses '"a: 2, b: hello"'
-- filtersP :: ParserT String Identity (SM.StrMap String)
-- filtersP = strMap queryParser.identifier

valueFiltersP :: ParserT String Identity ValuesFilter
valueFiltersP = strMap queryParser.natural

-- Parses "sales: D"
sortP :: ParserT String Identity Sort
sortP = map toSort $ propTuple queryParser.identifier (map (const ASC) (string "A") <|> map (const DESC) (string "D"))
  where
    toSort (Tuple a b) = Sort {by: a, order: b}

-- Parses "(sales:A,[views:100,sales:1])"
breakdownSortAndValuesP :: ParserT String Identity BreakdownDetails -- (Tuple (Maybe Sort) (Maybe (SM.StrMap Int)))
breakdownSortAndValuesP = map toDetails $ map (bimap Just Just) (try $ tuple sortP valueFiltersP) <|> map toTuple (queryParser.parens sortP)
  where toTuple x = Tuple (Just x) Nothing
        toDetails (Tuple s v) = BreakdownDetails { sort: s, valuesFilter: v }

-- Parses "country_code:(sales:A),operator_code,date:(views:A,[sales:10,views:100])"
breakdownP :: ParserT String Identity Breakdown
breakdownP = emptyStrMap emptyBreakdownDetails breakdownSortAndValuesP <* eof

---

filterValP :: ParserT String Identity FilterVal
filterValP = 
      FilterValLike <$> (try (star <+> queryParser.identifier <+> star) <|> try star <+> queryParser.identifier <|> try (queryParser.identifier <+> star))
  <|> FilterValUnquotedInt <$> try (signInt <*> queryParser.natural <* notFollowedBy (string "."))
  <|> FilterValUnquotedNumber <$> (signNum <*> queryParser.float)
  <|> FilterValStr <$> queryParser.identifier

  where
    signInt = queryParser.reservedOp "+" $> id <|> queryParser.reservedOp "-" $> (\x -> -1 * x)
    signNum = queryParser.reservedOp "+" $> id <|> queryParser.reservedOp "-" $> (\x -> toNumber(-1) * x)
    star = queryParser.reservedOp "*" $> "%"

concatP :: ParserT String Identity String -> ParserT String Identity String -> ParserT String Identity String
concatP pa pb = (<>) <$> pa <*> pb 
infixr 4 concatP as <+>

filterLangP :: ParserT String Identity FilterLang
filterLangP = 
      FilterIn <$> list filterValP
  <|> uncurry FilterRange <$> tuple filterValP filterValP
  <|> FilterEq <$> filterValP

filtersP :: ParserT String Identity Filters
filtersP = strMap filterLangP

{-
filterStr :: String
filterStr = "country_code:[ar,za,th,my,mx,om,qa],affiliate_id:POM*,publisher_id:[*1292*,122*],screen_width:(+200,+500),offer:+144,has_os:+1"

main = runParser filterStr filterP
-}