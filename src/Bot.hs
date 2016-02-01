{-module Bot ( module Bot.GameAPI
           , module Bot.TwitterAPI
           , runBot
           ) where -}
           
module Bot where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Text (Text)
import qualified Data.Text as T
import           Safe

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

runBot :: IO ()
runBot = flip evalStateT initialState . forever $ do
  s <- get
  prevID <- gets lastID
  previous <- gets killEvents
  newEvents <- liftIO $ getKills (Just prevID)
  put s { lastID = 0 maximumDef 0 $ map id_ $ map details newEvents
        , killEvents = newEvents ++ previous}
  liftIO . print $ previous
  liftIO . threadDelay $ 1000000 * 5

  
test :: IO ()
test = putStrLn "hello world"

