{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Function (on)
import           Data.List
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import qualified Database.Redis as DB
import           Network.HTTP.Client.Conduit
import           Safe
import           System.Environment (getEnv)
import           Web.Authenticate.OAuth hiding (delete)
import qualified Web.Twitter.Conduit as CT

import           Bot.GameAPI

main' :: IO ()
main' = do
  twInfo <- setupAuth
  mgr    <- newManager
  db     <- DB.connect DB.defaultConnectInfo
  runBot twInfo mgr db

runBot :: CT.TWInfo -> Manager -> DB.Connection -> IO ()
runBot twInfo mgr db = forever $ do
  updateEvents db
  expireOld db
  weeklyClasses db twInfo mgr
  weeklyPlayers db twInfo mgr
  dailyPlayers db twInfo mgr
  pauseFor oneMinute
  where pauseFor  = liftIO . threadDelay
        oneMinute = 60 * 1000 * 1000

checkFlag :: DB.Connection           -- ^ Redis connection
          -> String                  -- ^ Redis 'key' used for flag
          -> Frequency               -- ^ Time delay (seconds)
          -> (DB.Connection -> IO a) -- ^ Action if flag not found
          -> IO ()
checkFlag db k t f = do
  flag <- DB.runRedis db $ DB.get k'
  whenMissing (fromR flag) $ do
    f db
    DB.runRedis db $ DB.set k' "" >> DB.expire k' t >> return ()
  where k' = BS.pack k

top3 db range rangeStr f t twInfo mgr =
  checkFlag db rangeStr range $ \db -> do
    today <- liftIO getCurrentTime
    events <- DB.runRedis db getEvents
    fromRange <- forM (fromR events) $ \evtID -> do
      date <- DB.runRedis db $ getDate evtID
      case fromR date >>= parseAchaeaTime >>= inRange today of
        Nothing -> return Nothing
        Just _ -> return . Just $ evtID
    fList <- forM (catMaybes fromRange) $ \evtID -> do
      fRes <- DB.runRedis db $ f evtID
      case fromR fRes of
        Nothing -> return Nothing
        Just valid -> return . Just $ valid
    let counts = sortSndDesc . occurrences . catMaybes $ fList
        top3 = take 3 counts
        topNames = map (BS.unpack . fst) top3
        formatted = fmtList topNames
    runResourceT $ CT.call twInfo mgr $ CT.update . T.pack $ t ++ " " ++ formatted
  where
    inRange currentTime date =
      if diffUTCTime currentTime date < fromIntegral range
        then Just date
        else Nothing
    sortSndDesc = sortBy (flip compare `on` snd)

dailyPlayers db = top3 db daily "24hrPlayer" getKName
  "The deadliest adventurers during the last 24 hours were"

weeklyPlayers db = top3 db weekly "weekPlayer" getKName
  "The deadliest adventurers of last week were"

weeklyClasses db = top3 db weekly "weekClass" getKClass
  "The most OP classes of last week were"

getKClass evtID = DB.hget evtID "killerClass"
getKName evtID = DB.hget evtID "killerName"
getDate evtID = DB.hget evtID "date"
getEvents = DB.smembers "events"

occurrences xs = map (head &&& length) (group . sort $ xs)

fmtList :: [String] -> String
fmtList xs = fmt ++ "."
  where fmt = concat . reverse $ head commaSep:"and ":tail commaSep
        commaSep = reverse $ intersperse ", " xs

expireOld :: DB.Connection -> IO ()
expireOld db = checkFlag db "expireOld" daily $ \db -> do
  today <- liftIO getCurrentTime
  putStrLn "deleting old records."
  events <- DB.runRedis db getEvents
  forM_ (fromR events) (checkDate db today)
  where
    removeEvent evt = DB.del [evt] >> DB.srem "events" [evt] >> return ()
    checkDate db currentTime evtID = DB.runRedis db $ do
      date <- getDate evtID
      case fromR date >>= parseAchaeaTime of
        Nothing -> removeEvent evtID
        Just parsedDate ->
          when (diffUTCTime currentTime parsedDate > fromIntegral monthly) $
            removeEvent evtID

parseAchaeaTime =
  parseTimeM False defaultTimeLocale "%F %T" . BS.unpack

updateEvents :: DB.Connection -> IO ()
updateEvents db = do
  prevID <- DB.runRedis db $ DB.setnx "prevID" "0" >> DB.get "prevID"
  case fromR prevID of
    Nothing -> putStrLn "prevID not found (should never happen)"
    Just validID -> do
      let validID' = read . BS.unpack $ validID
      newEvents <- getKills (Just validID')
      whenPresent newEvents $ \validEvents -> do
        let idList = map (id_ . details) validEvents
            newID  = maximumDef validID' idList
            tweets = map printKill validEvents
        DB.runRedis db $ do
          DB.set "prevID" $ BS.pack . show $ newID
          DB.sadd "events" $ map (BS.pack . show) idList
          mapM_ storeEvent validEvents
        mapM_ (putStrLn . T.unpack) tweets

storeEvent :: DB.RedisCtx m f => GameEvent -> m ()
storeEvent evt = do
  let key = BS.pack . show $ id_ . details $ evt
  DB.hset key "date" $ T.encodeUtf8 . date . details $ evt
  DB.hset key "killerName"  $ getData killer name
  DB.hset key "killerCity"  $ getData killer city
  DB.hset key "killerClass" $ getData killer class_
  DB.hset key "victimName"  $ getData victim name
  DB.hset key "victimCity"  $ getData victim city
  DB.hset key "victimClass" $ getData victim class_
  return ()
  where getData char field = T.encodeUtf8 . field . char $ evt

printKill :: GameEvent -> Text
printKill x = T.concat [ "Oh snap! ", name . killer $ x, " just killed ", name . victim $ x, "!"]

--runResourceT $ call twInfo mgr $ apicall
setupAuth :: IO CT.TWInfo
setupAuth = do
  consumerKey    <- BS.pack <$> getEnv "ACHAEACONSUMERKEY"
  consumerSecret <- BS.pack <$> getEnv "ACHAEACONSUMERSECRET"
  token          <- BS.pack <$> getEnv "ACHAEATOKEN"
  tokenKey       <- BS.pack <$> getEnv "ACHAEATOKENSECRET"
  let oauth = CT.twitterOAuth { oauthConsumerKey    = consumerKey
                              , oauthConsumerSecret = consumerSecret }
      cred = newCredential token tokenKey
  return $ CT.setCredential oauth cred def

fromR :: Either DB.Reply a -> a
fromR (Left  resp) = error $ show resp
fromR (Right a)    = a

whenMissing :: Applicative f => Maybe a -> f () -> f ()
whenMissing Nothing f = f
whenMissing _       _ = pure ()

whenPresent :: Applicative f => Maybe a ->(a -> f ()) -> f ()
whenPresent (Just x) f = f x
whenPresent _        _ = pure ()

-- Times (in seconds) used for expire keys
type Frequency = Integer

minutely :: Frequency
hourly   :: Frequency
daily    :: Frequency
monthly  :: Frequency

minutely = 60
hourly   = minutely * 60
daily    = hourly   * 24
weekly   = daily    * 7
monthly  = daily    * 30
