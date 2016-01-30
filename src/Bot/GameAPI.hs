{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-
module Bot.GameAPI ( GameEvent (..)
                   , Online (..)
                   , Event (..)
                   , Character (..)
                   , getOnline
                   , getCharacter
                   , gameFeed
                   , getKills ) where
-}

module Bot.GameAPI where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics
import           Servant.API
import           Servant.Client

import           Bot.Parser


-- API definition

type AchaeaAPI =
       "characters.json" :> Get '[JSON] Online
  :<|> "characters" :> Capture "name" Text :> Get '[JSON] Character
  :<|> "gamefeed.json" :> Get '[JSON] [Event]

achaeaAPI :: Proxy AchaeaAPI
achaeaAPI = Proxy

getOnline :: EitherT ServantError IO Online
getCharacter' :: Text -> EitherT ServantError IO Character
getCharacter :: Text -> EitherT ServantError IO Character
getCharacter char = getCharacter' $ char `T.append` ".json"
gameFeed :: EitherT ServantError IO [Event]

getOnline :<|> getCharacter' :<|> gameFeed = client achaeaAPI (BaseUrl Http "api.achaea.com" 80)


-- "Online" is not really used for anything atm
data Online = Online { count :: Int
                     , characters :: [Text]
                     } deriving (Show)

instance FromJSON Online where
  parseJSON (Object o) = do
    count <- read <$> o .: "count"
    charInfo <- o .: "characters"
    let names = mapMaybe (parseMaybe $ \obj -> obj .: "name") charInfo
    return $ Online count names
  parseJSON _ = mempty

data Character = Character { name :: Text
                           , city :: Text --City
                           , house :: Text
                           , level :: Int
                           , class_ :: Text --Class
                           , playerKills :: Int
                           , mobKills :: Int
                           } deriving (Show)

instance FromJSON Character where
  parseJSON (Object o) = do
    name <- o .: "name"
    city <- o .:? "city" .!= "(none)"
    house <- o .:? "house" .!= "(none)"
    level <- (fmap . fmap) read (o .:? "level") .!= 0
    class_ <- o .:? "class" .!= "(unknown)"
    return $ Character name city house level class_ 0 0
  parseJSON _ = mempty

data EventType = DEA | LUP | LDN | DUE | NEW
  deriving (Show, Read, Eq, Generic)

instance FromJSON EventType

data Event = Event { id :: Int
                   , desc :: Text
                   , type_ :: EventType
                   , date :: Text --TODO: parse into Date
                   } deriving (Show, Eq)

instance FromJSON Event where
  parseJSON (Object o) = do
    id <- o .: "id"
    desc <- o .: "description"
    type_ <- o .: "type"
    date <- o .: "date"
    return $ Event id desc type_ date

  parseJSON _ = mempty

data GameEvent = GameEvent { details :: Event
                           , killer :: Character
                           , victim :: Character
                           } deriving (Show)

getKills :: IO (Either ServantError ())
getKills = runEitherT $ do
  feed <- gameFeed
  return ()

onlyDeaths = filter (\evt -> type_ evt == DEA)

testEvents = [testEvent1,testEvent2,testEvent3]
testEvent1 = Event 2342 "PersonA was slain by PersonB." DEA "Sometime"
testEvent2 = Event 123 "Hello was slain by Adele." DEA "Whenever"
testEvent3 = Event 666 "Nobody was killed." LUP "Never"

