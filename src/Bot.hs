{-module Bot ( module Bot.GameAPI
           , module Bot.TwitterAPI
           , runBot
           ) where -}

module Bot where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Redis
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
  runBot twInfo mgr

runBot :: CT.TWInfo -> Manager -> IO ()
runBot twInfo mgr = flip evalStateT initialState . forever $ do
  s <- get
  prevID <- gets lastID
  previous <- gets killEvents
  newEvents <- liftIO $ getKills (Just prevID)
  let newID = maximumDef prevID $ map (id_ . details) newEvents
  put s { lastID = newID
        , killEvents = nub $ newEvents ++ previous}
  let tweets = map printKill newEvents
  liftIO $ mapM (putStrLn . T.unpack) tweets
  -- Pause for a minute
  liftIO . threadDelay $ 1000000 * 60

--runResourceT $ call twInfo mgr $ apicall

test :: IO ()
test = putStrLn "hello world"

printKill :: GameEvent -> Text
printKill x = T.pack $ "Oh snap! " ++ show (name . killer $ x) ++ " just killed " ++ show (name . victim $ x) ++ "!"
