{-# LANGUAGE OverloadedStrings #-}

module Bot.Parser (getPlayersFromDesc
                  ) where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 (space, string, char, many1)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- Takes a string like "Romeo was slain by Tecton."
-- and returns (Killer, Victim) aka ("Tecton", "Romeo")
getPlayersFromDesc :: Text -> Either String (Text, Text)
getPlayersFromDesc = parseOnly parsePlayers . T.encodeUtf8

parsePlayers :: Parser (Text, Text)
parsePlayers = do
  victim <- name
  space
  string "was slain by"
  space
  killer <- name
  char '.'
  case killer of
    "misadventure" -> fail "not a player"
    _ -> return $ mapTuple T.pack (killer, victim)
  where mapTuple f (a,b) = (f a, f b)
        name = many1 letter_ascii


