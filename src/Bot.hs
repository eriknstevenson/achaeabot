{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
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

main' :: IO ()
main' = do
  twInfo <- setupAuth
  mgr <- newManager
  db <- DB.connect DB.defaultConnectInfo
  runBot twInfo mgr db

runBot :: CT.TWInfo -> Manager -> DB.Connection -> IO ()
runBot twInfo mgr db = forever $ do
  updateEvents db
  expireOld db
  pauseFor oneMinute

--microseconds used for threaddelay
pauseFor = liftIO . threadDelay
oneMinute = 1000000 * 60

--seconds used for redis expire
minutely = 60
hourly = minutely * 60
daily = hourly * 24
monthly = fromIntegral $ daily * 30

-- delete events > 30 days old, performed daily
expireOld db = do
  today <- getCurrentTime
  dbResp <- DB.runRedis db $ DB.get "expireOld"
  case dbResp of
    Left reply -> print reply
    Right flag ->
      case flag of
        Nothing -> do
          --flag expired, need to redo the task
          putStrLn "deleting old records."
          dbResp <- DB.runRedis db $ DB.smembers "events"
          case dbResp of
            Left reply -> print reply
            Right events -> do
              flip mapM_ events $ \evt ->
                DB.runRedis db $ do
                  dbResp <- DB.hget evt "date"
                  case dbResp of
                    Left reply -> liftIO $ print reply
                    Right date -> do
                      case date of
                        Nothing -> removeEvent evt
                        Just validDate -> do
                          case parseAchaeaTime validDate of
                            Nothing -> removeEvent evt
                            Just eventTime ->
                              case diffUTCTime today eventTime > monthly of
                                False -> return ()
                                True -> removeEvent evt
          --set the flag again
          DB.runRedis db $ do
            DB.set "expireOld" "false" --value doesn't really matter
            DB.expire "expireOld" minutely
          return ()
        Just validFlag -> return ()

removeEvent evt = do
  DB.del [evt]
  DB.srem "events" [evt]
  return ()

parseAchaeaTime :: ByteString -> Maybe UTCTime
parseAchaeaTime =
  (parseTimeM False defaultTimeLocale "%F %T") . BS.unpack

updateEvents db = do
  dbResp <- DB.runRedis db $ DB.setnx "prevID" "0" >> DB.get "prevID"
  case dbResp of
    Left reply -> print reply
    Right prevID ->
      case prevID of
        Nothing -> putStrLn "prevID not found (should never happen)"
        Just validID -> do
          let validID' = read . BS.unpack $ validID
          newEvents <- getKills (Just validID')
          case newEvents of
            Nothing -> putStrLn "something went wrong while fetching data."
            Just validEvents -> do
              let idList = map (id_ . details) validEvents
                  newID = maximumDef validID' idList
              DB.runRedis db $ do
                DB.set "prevID" $ BS.pack . show $ newID
                DB.sadd "events" $ map (BS.pack . show) idList
                mapM_ storeEvent validEvents
              let tweets = map printKill validEvents
              mapM_ (putStrLn . T.unpack) tweets

storeEvent evt = do
  let key = BS.pack . show $ id_ . details $ evt
  DB.hset key "date" $ T.encodeUtf8 . date . details $ evt
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
