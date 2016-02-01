{-# LANGUAGE OverloadedStrings #-}

module Bot.TwitterAPI where

import           Control.Monad.IO.Class
import           Control.Lens
import qualified Data.ByteString.Char8 as BS
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Conduit
import           System.Environment
import           Web.Authenticate.OAuth
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens

getTokens :: IO OAuth
getTokens = do
  consumerKey <- getEnv "ACHAEAOAUTHKEY"
  consumerSecret <- getEnv "ACHAEAOAUTHSECRET"
  return $
    twitterOAuth { oauthConsumerKey = BS.pack consumerKey
                 , oauthConsumerSecret = BS.pack consumerSecret
                 }

