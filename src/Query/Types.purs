module Query.Types where

import Prelude
import Data.Foreign
import Data.Foreign.Class
import Data.JSDate as JSD
import Data.StrMap as SM
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (runExcept)
import Data.Foreign.Index ((!))
import Data.Function.Uncurried (Fn2, Fn1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.JSDate (JSDate, toTimeString)
import Data.List (List(..), intercalate, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.StrMap (StrMap, fold, lookup)
import Data.Traversable (foldl, traverse)
   
data SortOrder = ASC | DESC
derive instance genericSortOrder :: Generic SortOrder _
instance showSortOrder :: Show SortOrder where
  show = genericShow

data Sort = Sort {
  by :: String,
  order :: SortOrder
}
derive instance genericSort :: Generic Sort _
instance showSort :: Show Sort where
  show = genericShow

type ValuesFilter = StrMap Int

data BreakdownDetails = BreakdownDetails {
  sort :: Maybe Sort,
  valuesFilter :: Maybe ValuesFilter
}
derive instance genericBreakdownDetails :: Generic BreakdownDetails _
instance showBreakdownDetails :: Show BreakdownDetails where
  show = genericShow

emptyBreakdownDetails :: BreakdownDetails
emptyBreakdownDetails = BreakdownDetails { sort: Nothing, valuesFilter: Nothing }

type Breakdown = StrMap BreakdownDetails

data FilterVal = FilterValStr String | FilterValLike String | FilterValUnquotedInt Int | FilterValUnquotedNumber Number
derive instance genericFilterVal :: Generic FilterVal _
instance showFilterVal :: Show FilterVal where
  show = genericShow

data FilterLang = 
    FilterIn (List FilterVal) 
  | FilterEq FilterVal
  | FilterRange FilterVal FilterVal

derive instance genericFilterLang :: Generic FilterLang _
instance showFilterLang :: Show FilterLang where
  show = genericShow


type Filters = StrMap FilterLang

newtype QueryParams d = QueryParams {
  timezone :: Number,
  dateFrom :: d,
  dateTo :: d,
  breakdown :: Breakdown,
  filters :: Filters
}

class ToSqlDateStr v where
  toSqlDateStr :: v -> String

instance stringToSqlDateStr :: ToSqlDateStr String where
  toSqlDateStr = toSqlDateStr <<< unsafePerformEff <<< JSD.parse

instance jsDateTimeToSqlDateStr :: ToSqlDateStr JSDate where
  toSqlDateStr = unsafePerformEff <<< JSD.toISOString

readQueryParams :: Foreign -> F (QueryParams String)
readQueryParams value = do
  timezone <- fromMaybe (toNumber 0) <$> (value ! "timezone" ?>>= readNumber)
  dateFrom <- value ! "dateFrom" >>= readString --unsafePerformEff (JD.toISOString  =<< JD.parse "2018-11-02")
  dateTo <- value ! "dateTo" >>= readString
  breakdown <- unsafeFromForeign <$> (value ! "breakdown")
  filters <- unsafeFromForeign <$> (value ! "filters")
  pure $ QueryParams { timezone, dateFrom, dateTo, breakdown, filters }


---  Sql Template

newtype QueryOptions = QueryOptions {
  noTimezone :: Boolean,
  tableAlias :: String,
  timeColName :: String,
  fieldMap :: StrMap String,
  casted :: Boolean
}
derive instance genericQueryOptions :: Generic QueryOptions _
instance showQueryOptions :: Show QueryOptions where
  show = genericShow

readQueryOptions :: Foreign -> F QueryOptions
readQueryOptions value = do
  noTimezone <- fromMaybe false <$> (value ! "noTimezone" ?>>= readBoolean)
  tableAlias <- value ! "tableAlias" >>= readString
  timeColName <- value ! "timeColName" >>= readString
  fieldMap <- unsafeFromForeign <$> (value ! "fieldMap" )
  casted <- fromMaybe false <$> (value ! "casted" ?>>= readBoolean)
  pure $ QueryOptions { noTimezone, tableAlias, timeColName, fieldMap, casted }


breakdownToSqlSelect :: forall d. String -> QueryParams d -> QueryOptions -> String
breakdownToSqlSelect indent params@(QueryParams p) options@(QueryOptions q) = 
  "  " <> (intercalate newLine $ fold (\ls k _ -> ((if q.casted then (alias' <<< dimension) else defaultCast) k) : ls) mempty p.breakdown)
  where
    alias' = alias options

    defaultCast :: String -> String
    defaultCast c = as c $ go c where
      go "hour" = timeDim "hour"
      go "day" = timeDim "day"
      go "week" = timeDim "week"
      go "month" = timeDim "month"
      go col = 
        let col' = fromMaybe col $ SM.lookup col q.fieldMap
        in "coalesce(cast(" <> alias' col' <> " as varchar), 'Unknown')"

      as col expr = expr <> " as " <> dimension col
      timeDim dim = "date_trunc('" <> dim <> "', CONVERT_TIMEZONE('UTC', '" <> tz <> "', " <> alias' q.timeColName <> ")) :: timestamp AT TIME ZONE '" <> tz <> "'"
      tz = show $ toNumber(-1) * p.timezone
      -- timezone conversion example:  date_trunc('day', CONVERT_TIMEZONE('UTC', '-8', e.timestamp)) :: timestamp AT TIME ZONE '-8' 

    newLine = "\n" <> indent <> ", "

breakdownToSqlCommaSep :: Maybe QueryOptions -> Breakdown -> String
breakdownToSqlCommaSep moptions = intercalate ", " <<< fold (\ls k _ -> alias' (dimension k) : ls) mempty
  where
    alias' = case moptions of
      Just options -> alias options
      _ -> id

alias :: QueryOptions -> String -> String
alias (QueryOptions q) col = q.tableAlias <> "." <> col

joinDimensionsToSqlJoin :: forall d. String -> QueryParams d -> QueryOptions -> QueryOptions -> String
joinDimensionsToSqlJoin indent params@(QueryParams p) qLeft qRight = 
  "    " <> (intercalate newLine $ fold (\ls k _ -> ((alias qLeft <<< dimension) k  <> " = " <> (alias qRight <<< dimension) k ) : ls) mempty p.breakdown)
  where
    newLine = "\n" <> indent <> "AND "

dimension :: String -> String
dimension col = "d_" <> col


filtersToSqlWhere :: forall d. ToSqlDateStr d => String -> QueryParams d -> QueryOptions -> String
filtersToSqlWhere indent params@(QueryParams p) options@(QueryOptions q) = intercalate (newLine <> "AND ") (timeStr:rest)
  where
    alias' = alias options
    rest = filtersToSqls params options
    timeStr = "    " <> alias' q.timeColName <> " >= " <> inSq (toSqlDateStr p.dateFrom) <> newLine <> "AND " <> alias' q.timeColName <> " < " <> inSq (toSqlDateStr p.dateTo)
    newLine = "\n" <> indent

filtersToSqlConds :: forall d. ToSqlDateStr d => String -> QueryParams d -> QueryOptions -> String
filtersToSqlConds indent params options = intercalate (newLine <> "AND ") $ filtersToSqls params options
  where
    newLine = "\n" <> indent

filtersToSqls :: forall d. ToSqlDateStr d => QueryParams d -> QueryOptions -> List String
filtersToSqls params@(QueryParams p) options@(QueryOptions q) = rest
  where
    alias' = alias options
    rest = map (\x -> "(" <> x <> ")") $ fold (\ls k v -> filterLangToStr (alias' k) v : ls) mempty p.filters

    filterLangToStr :: String -> FilterLang -> String
    filterLangToStr col (FilterIn vals) = intercalate " OR " $ map ((\v -> col <> v) <<< filterValToStr) vals 
    filterLangToStr col (FilterEq val) =  col  <> filterValToStr val
    filterLangToStr col (FilterRange a b) = col <> " >= "  <> filterValToRangeStr a <> " AND " <> col <> " < " <> filterValToRangeStr b

    filterValToStr :: FilterVal -> String
    filterValToStr (FilterValStr s) = " = " <> inSq s
    filterValToStr (FilterValLike s) = " LIKE " <> inSq s
    filterValToStr (FilterValUnquotedInt i) = " = " <> show i
    filterValToStr (FilterValUnquotedNumber i) = " = " <> show i

    filterValToRangeStr :: FilterVal -> String
    filterValToRangeStr (FilterValUnquotedInt i) = show i
    filterValToRangeStr (FilterValUnquotedNumber i) = show i
    filterValToRangeStr x = " NOT SUPPORTED " <> show x


inSq :: String -> String
inSq s = "'" <> s <> "'"


---
 
foreign import sayHello :: String
foreign import toSql :: forall v. (StrMap v -> String) -> String
foreign import evalJs :: forall v. v -> String


newtype Cat = Cat { color :: Maybe String, breed :: Maybe String }
derive instance genericCat :: Generic Cat _
instance showCat :: Show Cat where
  show = genericShow

readCat :: Foreign -> F Cat
readCat value = do
  color <- value ! "color" ?>>= readString
  breed <- value ! "breed" ?>>= readString
  pure $ Cat { color: color, breed: breed }


tryParse :: forall a. F Foreign -> (Foreign -> F a) -> F (Maybe a)
tryParse v r = v >>= readNullOrUndefined >>= traverse r
infixl 4 tryParse as ?>>=

foreign import myCat :: Cat

emptyCat = Cat { color: Nothing, breed: Nothing }
foreign import mkCat :: forall a. Fn2 Cat (a -> Maybe a) Cat

{-
import Text.Parsing.Parser (runParser)
import Query.Parser.UrlQueryParser
b =  runParser "country_code:(sales:A),operator_code,day:(views:A,[sales:10,views:100])" breakdownP
breakdownToSqlSelect "us" "timestamp"  <$> b
-}