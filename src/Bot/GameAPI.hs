{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.GameAPI ( getKills 
                   , GameEvent (..) ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           GHC.Generics
import           Servant.API
import           Servant.Client

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
    level <- read <$> o .: "level"
    class_ <- o .: "class"
    playerKills <- read <$> o .: "player_kills"
    mobKills <- read <$> o .: "mob_kills"
    return $ Character name city house level class_ playerKills mobKills
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

getKills :: IO [GameEvent]
getKills = do
  feed <- runEitherT gameFeed
  case feed of
    Right goodData -> do
      liftM catMaybes . sequence $ map makeGameEvent (onlyDeaths goodData)
    Left _ -> error "Invalid API request"
  where
    onlyDeaths = filter (\evt -> type_ evt == DEA)

makeGameEvent :: Event -> IO (Maybe GameEvent)
makeGameEvent evt = do
  k <- getKInfo evt
  v <- getVInfo evt
  case (k, v) of
    (Just k', Just v') -> return . Just $ GameEvent evt k' v'
    _ -> return Nothing

getKInfo = getCharInfo extractKiller
getVInfo = getCharInfo extractVictim

getCharInfo :: (Event -> Maybe Text) -> Event -> IO (Maybe Character)
getCharInfo f evt = do
  case (f evt) of
    Nothing -> return Nothing
    Just validName -> do
      info <- runEitherT $ getCharacter validName
      case info of
        Left _ -> return Nothing
        Right validInfo -> return $ Just validInfo

extractKiller = getNameFromEvent fst
extractVictim = getNameFromEvent snd

getNameFromEvent :: ( (Text, Text) -> a ) -> Event -> (Maybe a)
getNameFromEvent f evt = case runParser evt of
  Left _ -> Nothing
  Right validParse -> Just (f validParse)

runParser :: Event -> Either String (Text, Text)
runParser = A.parseOnly parsePlayers . T.encodeUtf8 . desc

parsePlayers :: A.Parser (Text, Text)
parsePlayers = do
  victim <- name
  A.space
  A.string "was slain by"
  A.space
  killer <- name
  A.char '.'
  case killer of
    "misadventure" -> fail "not a player"
    _ -> return $ mapTuple T.pack (killer, victim)
  where mapTuple f (a,b) = (f a, f b)
        name = A.many1 A.letter_ascii
        
