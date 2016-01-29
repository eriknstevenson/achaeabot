{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.GameAPI ( Online
                   , GameEvent
                   , Character
                   , getOnline
                   , getCharacter
                   , getEvents ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client
import qualified Data.Text as T
import qualified Data.Text.IO as T

type AchaeaAPI =
       "characters.json" :> Get '[JSON] Online
  :<|> "characters" :> Capture "name" Text :> Get '[JSON] Character
  :<|> "gamefeed.json" :> Get '[JSON] [GameEvent]

data Online = Online { count :: Int
                     , characters :: [Text]
                     } deriving (Show)

instance FromJSON Online where
  parseJSON (Object o) = do
    count <- read <$> o .: "count"
    charInfo <- o .: "characters"
    let names = flip mapMaybe charInfo $ parseMaybe $ \obj -> obj .: "name"
    return $ Online count names
  parseJSON _ = mempty

data City = Hashan | Mhaldor | Ashtan | Cyrene | Targossas | None
            deriving (Show, Eq, Read, Generic)

instance FromJSON City

data Character = Character { name :: Text
                           , city :: City
                           , level :: Int
                           , class_ :: Text --Class
                           , playerKills :: Int
                           , mobKills :: Int
                           } deriving (Show)

-- TODO: getOnline only returns name and zeros out everything else
-- when updating the db, ignore the field if it's 0? mappend
instance FromJSON Character where
  parseJSON (Object o) = do
    name <- o .: "name"
    city <- o .:? "city" .!= None
    level <- (fmap . fmap) read (o .:? "level") .!= 0
    class_ <- o .:? "class" .!= ""
    return $ Character name city level class_ 0 0
  parseJSON _ = mempty

data EventType = DEA | LUP | LDN | DUE | NEW deriving (Show, Read, Eq, Generic)

instance FromJSON EventType

data GameEvent = GameEvent { id :: Int
                           , desc :: Text
                           , type_ :: EventType
                           , date :: Text --TODO: parse into Date
                           } deriving (Show, Eq)

instance FromJSON GameEvent where
  parseJSON (Object o) = do
    id <- o .: "id"
    desc <- o .: "description"
    type_ <- o .: "type"
    date <- o .: "date"
    return $ GameEvent id desc type_ date

  parseJSON _ = mempty

achaeaAPI :: Proxy AchaeaAPI
achaeaAPI = Proxy

getOnline :: EitherT ServantError IO Online
getCharacter :: Text -> EitherT ServantError IO Character
getEvents :: EitherT ServantError IO [GameEvent]
getOnline :<|> getCharacter :<|> getEvents = client achaeaAPI (BaseUrl Http "api.achaea.com" 80)

test :: IO (Either ServantError ())
test = runEitherT $ do
  chars <- getOnline
  liftIO . putStrLn $ show (count chars) ++ " characters"

  return ()


