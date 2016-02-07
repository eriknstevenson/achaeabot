{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
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
  weeklyTop3Classes db
  pauseFor oneMinute
  where pauseFor  = liftIO . threadDelay
        oneMinute = 60 * 1000 * 1000

checkFlag :: DB.Connection           -- ^ Redis connection
          -> ByteString              -- ^ Redis 'key' used for flag
          -> Frequency               -- ^ Time delay (seconds)
          -> (DB.Connection -> IO a) -- ^ Action if flag not found
          -> IO ()
checkFlag db k t f = do
  flag <- DB.runRedis db $ DB.get k
  whenMissing (fromR flag) $ do
    f db
    DB.runRedis db $ DB.set k "" >> DB.expire k t >> return ()


weeklyTop3Players db = checkFlag db "weeklyTopPlayer" weekly $ \db -> do
  today <- liftIO getCurrentTime
  events <- DB.runRedis db getEvents
  fromLastWeek <- forM (fromR events) $ \evtID -> do
    date <- DB.runRedis db $ getDate evtID
    case fromR date >>= parseAchaeaTime >>= lastWeek today of
      Nothing -> return Nothing
      Just _ -> return . Just $ evtID
  killerNames <- forM (catMaybes fromLastWeek) $ \evtID -> do
    kName <- DB.runRedis db $ getKName evtID
    case fromR kName of
      Nothing -> return Nothing
      Just validName -> return . Just $ validName
  let counts = sortSndDesc . occurrences . catMaybes $ killerNames
      top3 = take 3 counts
      killers = map (BS.unpack . fst) top3
      opPlayers = fmtList killers
  putStrLn $ "The most OP adventurers last week were " ++ opPlayers
  where
    lastWeek currentTime date =
      if diffUTCTime currentTime date < fromIntegral weekly
         then Just date
         else Nothing
    sortSndDesc = sortBy (flip compare `on` snd)

weeklyTop3Classes db = checkFlag db "weeklyTopClass" weekly $ \db -> do
  today <- liftIO getCurrentTime
  events <- DB.runRedis db getEvents
  fromLastWeek <- forM (fromR events) $ \evtID -> do
    date <- DB.runRedis db $ getDate evtID
    case fromR date >>= parseAchaeaTime >>= lastWeek today of
      Nothing -> return Nothing
      Just _ -> return . Just $ evtID
  classesUsed <- forM (catMaybes fromLastWeek) $ \evtID -> do
    classUsed <- DB.runRedis db $ getKClass evtID
    case fromR classUsed of
      Nothing -> return Nothing
      Just validClass -> return . Just $ validClass
  let counts = sortSndDesc . occurrences . catMaybes $ classesUsed
      top3 = take 3 counts
      classNames = map (BS.unpack . fst) top3
      opClasses = fmtList classNames
  putStrLn $ "The most OP classes of the past week are " ++ opClasses
  where
    lastWeek currentTime date =
      if diffUTCTime currentTime date < fromIntegral weekly
         then Just date
         else Nothing
    sortSndDesc = sortBy (flip compare `on` snd)

getKClass evtID = DB.hget evtID "killerClass"
getKName evtID = DB.hget evtID "killerName"
getDate evtID = DB.hget evtID "date"
getEvents = DB.smembers "events"

occurrences xs = map (head &&& length) (group . sort $ xs)

fmtList :: [String] -> String
fmtList xs = fmt ++ "."
  where fmt = concat . reverse $ head commaSep:"and ":tail commaSep
        commaSep = reverse $ intersperse ", " xs

-- delete events > 30 days old, checked daily
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
