{-# LANGUAGE OverloadedStrings #-}

module Bot.Parser (getPlayersFromDesc
                  ) where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 (space, string, char, many1)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- Takes a string like "Tecton was slain by Romeo."
-- and returns (Victim, Killer) aka ("Juliet", "Romeo")
getPlayersFromDesc :: Text -> Result (Text, Text)
getPlayersFromDesc = parse parsePlayers . T.encodeUtf8

parsePlayers :: Parser (Text, Text)
parsePlayers = do
  victim <- word
  space
  string "was slain by"
  space
  killer <- word
  char '.'
  return $ mapTuple T.pack (victim, killer)
    where mapTuple f (a,b) = (f a, f b)
          word = many1 letter_ascii


