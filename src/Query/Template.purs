module Query.Template where
  
import Control.Monad.Aff
import Control.Monad.Aff.Console
import Control.Monad.Eff
import Prelude
import Data.Either
import Data.StrMap as SM
import Data.String.Regex as R
import Node.FS.Aff as FS
import Query.Parser.TemplateParser as TP
import Control.Monad.Aff.Console (log)
import Data.Either (either)
import Data.Int (toNumber)
import Data.StrMap (fold)
import Node.Encoding (Encoding(..))
import Query.Types (Breakdown)


tselect :: String -> String -> Breakdown -> String
tselect alias timecol = SM.fold (\z s a -> "") ""



main1 :: forall t24.
  Eff
    ( fs :: FS.FS
    , console :: CONSOLE
    | t24
    )
    Unit
main1 = do
  let filtersStr = "country_code:[AE,ZA,TH,MY,MX,OM,QA],affiliate_id:POM*"
  let breakdownStr = "country_code:(sales:D),publisher_id:(sales:D),day:(views:D,[sales:10,views:100])"
  void $ launchAff $ do
    text <- FS.readTextFile UTF8 "./src/Reports/Sessions/template.sql"
    case (
      TP.doTemplate 
        filtersStr
        breakdownStr
        (toNumber 2)
        "2018-03-01"
        "2018-04-01"
        text
    )
      of
        Left e -> log $ show e
        Right sql -> log sql
    -- log text
  -- liftEff' $ print text
  -- log text
  -- log "hello"

  -- let text' = R.replace' <$> (R.regex "\\{\\$(.*?)\\$\\}" (R.parseFlags "gi")) <*> (pure $ \ s _ -> "A") <*> (pure text)
  -- either log log text'
