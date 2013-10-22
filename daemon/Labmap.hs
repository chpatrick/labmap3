{-# LANGUAGE OverloadedStrings #-}

module Main where

import Labmap.Common
import Labmap.GetUser
import Labmap.Scanner
import Labmap.Users
import Labmap.Util

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.Aeson
import Data.Data
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.IORef
import Data.Text (Text)
import qualified Data.Map as M
import System.Environment
import System.Exit
import System.Posix.Files

data LabmapConf = LabmapConf
  { sshOpts :: [ String ]
  , machines :: Machines
  , outputFile :: FilePath
  } deriving (Read, Show)

instance FromJSON LabmapConf where
  parseJSON (Object conf)
    = LabmapConf <$>
        (words <$> (conf .: "sshOpts")) <*>
	(H.toList <$> (conf .: "machines")) <*>
	conf .: "outputFile"

instance ToJSON User where
  toJSON u = object
    [ "username" .= username u
    , "fullName" .= fullName u
    , "photo" .= photo u
    , "groups" .= groups u
    ]

findSelf :: IO FilePath
findSelf = readSymbolicLink "/proc/self/exe"

main :: IO ()
main = do
  as <- getArgs
  case as of
    [ "scan" ] -> scanCommand
    [ "getuser" ] -> getUser >>= print
    _ -> putStrLn "usage: labmap scan/getuser"

loadConfig :: IO LabmapConf
loadConfig = do
  configText <- BSL8.readFile "labmap.conf"
  case decode' configText of
    Nothing -> putStrLn "Failed to parse configuration." >> exitFailure
    Just c -> return c

scanCommand :: IO ()
scanCommand = do
  config <- loadConfig
  users <- cache (hours 5) getAllUsers
  labState <- newIORef (M.empty)
  resultChan <- newChan
  self <- findSelf
  scan (sshOpts config) (machines config) [ self, "getuser " ] resultChan 8
  forever $ do
    ( m, s ) <- readChan resultChan
    s' <- case s of
      Nothing -> return "UNKNOWN"
      Just Available -> return "AVAILABLE"
      Just (Occupied un) -> do
	ui <- M.lookup un <$> getCached users
	return $ case ui of
	  Nothing -> "UNKNOWN"
	  Just ui -> toJSON ui
    ls <- readIORef labState
    let ls' = M.insert m s' ls
    writeIORef labState ls'
    BSL8.writeFile (outputFile config) (encode ls')
