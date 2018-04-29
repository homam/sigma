module Server.Query where

import Control.Monad.Aff (Aff, Fiber, error, forkAff, killFiber, launchAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef)
import Prelude (class Show, Unit, bind, discard, pure, show, void, ($), (<$>), (<>))
import Data.Maybe (Maybe(..))
import Data.JSDate as JSDate
import Data.Map as M
import Database.Postgres as PG
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Data.Foreign.Generic (encodeJSON)

type QueryCache a = M.Map String (QueryState a)

data QueryState e = Running ( Fiber (FiberEffect e) Unit) JSDate.JSDate | Error String | Cancelled | Done (Array Foreign)

instance showQueryState ::  Show (QueryState a) where
  show (Running a b) = "Running"
  show (Error e) = "Error " <> e
  show Cancelled = "Cancelled"
  show (Done as) = "Done \n" <> encodeJSON as

type FiberEffect e = (db :: PG.DB, console :: CONSOLE, ref :: REF, now :: NOW | e)

type RunDbQuery b c =  
    String
  -> Aff ( db :: PG.DB | b ) c
    
type RunDbQueryAsync e = 
    String 
  → String 
  → Aff (FiberEffect e) Unit

type KillQuery e = 
    String 
  → Eff (FiberEffect e) (Either String String)

type GetQueryState e fb = 
    String 
  → Eff (ref :: REF | e) (Maybe (QueryState fb))

queryAsync :: ∀ e. 
    Ref (M.Map String (QueryState e))
  → PG.Pool 
  → RunDbQueryAsync e
queryAsync cache pool qid q = do
  let myAff = PG.withClient pool (PG.query_ (PG.Query q :: PG.Query Foreign))

  fiber <- forkAff $ do
    results <- myAff
    liftEff $ log $ "Done " <> qid
    liftEff $ modifyRef cache (M.insert qid $ Done results)

  now <- liftEff JSDate.now
  liftEff $ modifyRef cache (M.insert qid $ Running fiber now)


getQueryState :: ∀ a e.Ref (M.Map String (QueryState a)) → String → Eff ( ref ∷ REF | e ) (Maybe (QueryState a))
getQueryState cache qid = M.lookup qid <$> readRef cache 

killQuery :: ∀ a. Ref (M.Map String (QueryState a)) → KillQuery a
killQuery cache qid = do
  mqs <-  getQueryState cache qid
  case mqs of
    Nothing -> pure $ Left $ "No query was found for " <> qid
    Just qs -> case qs of
      Running fiber _ -> do
        void $ launchAff $ killFiber (error "User Cancelled") fiber
        modifyRef cache (M.insert qid $ Cancelled)
        pure $ Right "Killed"
      x -> pure $ Left $ "Invalid Query State: " <> show x