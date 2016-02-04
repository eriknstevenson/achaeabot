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
import qualified Data.Text.Encoding as T
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

data BotState = BotState { lastID :: Int }
                         deriving (Show)

initialState :: BotState
initialState = BotState 0

main' :: IO ()
main' = do
  twInfo <- setupAuth
  mgr <- newManager
  db <- DB.connect DB.defaultConnectInfo
  runBot twInfo mgr db

-- TODO: remove use of state monad and store lastID in the redis db
runBot :: CT.TWInfo -> Manager -> DB.Connection -> IO ()
runBot twInfo mgr db = flip evalStateT initialState . forever $ do
  s <- get
  prevID <- gets lastID
  maybeNewEvents <- liftIO $ getKills (Just prevID)
  case maybeNewEvents of
    Just newEvents -> do
      let idList = map (id_ . details) newEvents
          newID = maximumDef prevID idList
      put s { lastID = newID }
      liftIO . DB.runRedis db $ do
        eventsRes <- DB.sadd "events" $ map (BS.pack . show) idList
        mapM_ storeEvent newEvents
      let tweets = map printKill newEvents
      liftIO $ mapM_ (putStrLn . T.unpack) tweets
    Nothing -> liftIO . putStrLn $ "something went wrong while fetching data."
  -- Pause for a minute
  pauseFor oneMinute

pauseFor = liftIO . threadDelay
oneMinute = 1000000 * 60

--storeEvent :: GameEvent -> RedisTx (Queued ByteString)
storeEvent evt = do
  let key = BS.pack . show $ id_ . details $ evt
  DB.hset key "killerName" $ getData killer name
  DB.hset key "killerCity" $ getData killer city
  DB.hset key "killerClass" $ getData killer class_
  DB.hset key "victimName" $ getData victim name
  DB.hset key "victimCity" $ getData victim city
  DB.hset key "victimClass" $ getData victim class_
  where getData char field = T.encodeUtf8 . field . char $ evt

printKill :: GameEvent -> Text
printKill x = T.concat ["Oh snap! ", name . killer $ x, " just killed ", name . victim $ x, "!"]

--runResourceT $ call twInfo mgr $ apicall
--parseTimeM False defaultTimeLocale "%F %T" "2016-02-03 15:53:25"
