{-module Bot ( module Bot.GameAPI
           , module Bot.TwitterAPI
           , runBot
           ) where -}

module Bot where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Safe
import           Web.Authenticate.OAuth

import           Bot.GameAPI
import           Bot.TwitterAPI

{-
could store some statistics
such as # of kills by class, etc
but some of the statistics can be calculated
such as: getNumberOfKills :: [GameEvent] -> Int

consider building in 'getCharacter'-returned info into the
GameEvent object itself.
-}

type Bot = StateT BotState IO

data BotState = BotState { lastID :: Int
                         , killEvents :: [GameEvent] }
                         deriving (Show)

initialState :: BotState
initialState = BotState 0 []

main' :: IO ()
main' = do
  creds <- getTokens--get credentials
  runBot creds

runBot :: OAuth -> IO ()
runBot creds = flip evalStateT initialState . forever $ do
  s <- get
  prevID <- gets lastID
  previous <- gets killEvents
  newEvents <- liftIO $ getKills (Just prevID)
  let newID = maximumDef prevID $ map (id_ . details) newEvents
  put s { lastID = newID
        , killEvents = nub $ newEvents ++ previous}
  all <- gets killEvents
  liftIO . print $ length all
  liftIO . print $ newID
  liftIO . threadDelay $ 1000000 * 15

test :: IO ()
test = putStrLn "hello world"

