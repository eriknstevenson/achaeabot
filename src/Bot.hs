module Bot ( module Bot.GameAPI
           , module Bot.TwitterAPI
           ) where

import Bot.GameAPI
import Bot.TwitterAPI

{-
could store some statistics
such as # of kills by class, etc
but some of the statistics can be calculated
such as: getNumberOfKills :: [GameEvent] -> Int

consider building in 'getCharacter'-returned info into the
GameEvent object itself.
-}
data BotData = BotData { events :: [GameEvent]
               }


