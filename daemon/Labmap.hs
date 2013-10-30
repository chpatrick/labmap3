{-# LANGUAGE OverloadedStrings, MultiWayIf, LambdaCase, NamedFieldPuns #-}

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
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.Map as M
import qualified Data.Yaml as Yaml
import System.Environment
import System.Exit
import System.Log.Logger
import System.Posix.Files

data LabmapConf = LabmapConf
  { sshOpts :: [ String ]
  , machines :: Machines
  , outputFile :: FilePath
  , openingHour :: Int
  , closingHour :: Int
  , logLevel :: Priority
  } deriving (Read, Show)

instance FromJSON LabmapConf where
  parseJSON (Object conf)
    = LabmapConf <$>
        (words <$> (conf .: "sshOpts")) <*>
        (H.toList <$> (conf .: "machines")) <*>
        conf .: "outputFile" <*>
        conf .: "openingHour" <*>
        conf .: "closingHour" <*>
        (read <$> (conf .: "logLevel"))

instance ToJSON User where
  toJSON u = object
    [ "username" .= username u
    , "fullName" .= fullName u
    , "photo" .= photo u
    , "groups" .= groups u
    ]

findSelf :: IO FilePath
findSelf = readSymbolicLink "/proc/self/exe"

sleepTime :: Int -> Int -> IO (Maybe NominalDiffTime)
sleepTime open close = do
  now@(ZonedTime (LocalTime day (TimeOfDay h m s)) tz) <- getZonedTime
  return $ do
    wakeDay <- if
      | h < open -> Just day
      | h >= close -> Just (addDays 1 day)
      | otherwise -> Nothing
    let wakeTime = ZonedTime (LocalTime wakeDay (TimeOfDay open 0 0)) tz
    return $ zonedTimeToUTC wakeTime `diffUTCTime` zonedTimeToUTC now

main :: IO ()
main = do
  as <- getArgs
  case as of
    [ "scan" ] -> scanCommand
    [ "getuser" ] -> getUser >>= print
    _ -> putStrLn "usage: labmap scan/getuser"

loadConfig :: IO LabmapConf
loadConfig = do
  Yaml.decodeFile "labmap.conf" >>= \case
    Nothing -> putStrLn "Failed to parse configuration." >> exitFailure
    Just c -> return c

scanCommand :: IO ()
scanCommand = do
  LabmapConf { sshOpts, machines, outputFile, openingHour, closingHour, logLevel } <- loadConfig
  updateGlobalLogger "labmap" (setLevel logLevel)
  users <- cache (hours 5) getAllUsers
  labState <- newIORef (M.empty)
  resultChan <- newChan
  self <- findSelf
  scan sshOpts machines [ self, "getuser" ] resultChan 8
  noticeM "labmap" "Starting scan."
  forever $ do
    sleepTime openingHour closingHour >>= \case
      Just s -> do
        infoM "labmap" ("Sleeping for " <> show s)
        threadDelay (round (s * 1000000))
        infoM "labmap" "Woke up"
      Nothing -> return ()
    ( m, s ) <- readChan resultChan
    s' <- case s of
      Nothing -> return "UNKNOWN"
      Just Available -> return "AVAILABLE"
      Just (Occupied un) -> do
        ui <- M.lookup un <$> getCached users
        return $ case ui of
          Nothing -> "UNKNOWN"
          Just ui -> toJSON ui
    debugM "labmap" (T.unpack m <> ": " <> show s)
    ls <- readIORef labState
    let ls' = M.insert m s' ls
    writeIORef labState ls'
    BSL8.writeFile outputFile (encode ls')
