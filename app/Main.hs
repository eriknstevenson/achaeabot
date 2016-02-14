module Main where

import qualified Database.Redis as DB
import           Network.HTTP.Client.Conduit
import           Web.Authenticate.OAuth

import Bot

main :: IO ()
main = do
  twInfo <- setupAuth
  mgr    <- newManager
  db     <- DB.connect DB.defaultConnectInfo
  runBot twInfo mgr db
