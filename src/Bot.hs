{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Redis as DB
import           Network.HTTP.Client.Conduit
import           Safe
import qualified Web.Twitter.Conduit as CT

import           Bot.Database
import           Bot.GameAPI
import           Bot.TwitterAPI

{-
could store some statistics
such as # of kills by class, etc
but some of the statistics can be calculated
such as: getNumberOfKills :: [GameEvent] -> Int
-}

type Bot = StateT BotState IO

data BotState = BotState { lastID :: Int
                         , killEvents :: [GameEvent] }
                         deriving (Show)

initialState :: BotState
initialState = BotState 0 []

main' :: IO ()
main' = do
  twInfo <- setupAuth
  mgr <- newManager
  db <- DB.connect DB.defaultConnectInfo
  runBot twInfo mgr db

runBot :: CT.TWInfo -> Manager -> DB.Connection -> IO ()
runBot twInfo mgr db = flip evalStateT initialState . forever $ do
  s <- get
  prevID <- gets lastID
  previous <- gets killEvents
  newEvents <- liftIO $ getKills (Just prevID)
  let idList = map (id_ . details) newEvents
      newID = maximumDef prevID idList
  put s { lastID = newID
        , killEvents = nub $ newEvents ++ previous}
  liftIO . DB.runRedis db $ do
      --liftIO . putStrLn $ "adding new event ids."
      eventsRes <- DB.sadd "events" $ map (BS.pack . show) idList
      return ()
      --liftIO . putStrLn $ "doing other crap"
  let tweets = map printKill newEvents
  liftIO $ mapM (putStrLn . T.unpack) tweets
  -- Pause for a minute
  pauseFor oneMinute

--runResourceT $ call twInfo mgr $ apicall

--parseTimeM False defaultTimeLocale "%F %T" "2016-02-03 15:53:25"

test :: IO ()
test = putStrLn "hello world"

pauseFor = liftIO . threadDelay

oneMinute = 1000000 * 60

printKill :: GameEvent -> Text
printKill x = T.concat ["Oh snap! ", name . killer $ x, " just killed ", name . victim $ x, "!"]
