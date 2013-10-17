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
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.IORef
import Data.Text (Text)
import qualified Data.Map as M
import System.Environment
import System.Posix.Files

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

scanCommand :: IO ()
scanCommand = do
  users <- cache (hours 5) getAllUsers
  labState <- newIORef (M.empty)
  resultChan <- newChan
  self <- findSelf
  scan opts machines [ self, "getuser " ] resultChan 8
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
    BSL8.putStrLn $ encode ls'
