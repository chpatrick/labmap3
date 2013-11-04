{-# LANGUAGE OverloadedStrings, MultiWayIf, LambdaCase, RecordWildCards #-}

module Main where

import Labmap.Common
import Labmap.GetUser
import Labmap.Scanner
import Labmap.Users
import Labmap.Util

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.Map as M
import qualified Data.Yaml as Yaml
import Options.Applicative
import System.Log.Logger
import System.Posix.Files
import Web.Scotty as S

opts :: ParserInfo (IO ())
opts = info (helper <*> args) (fullDesc <> header "Labmap 3.0")
  where
    args = subparser $ 
      command "getuser" (info (pure getUserCommand) idm) <>
      command "server" (info (serverCommand <$>
        strOption (short 'c' <> value "labmap.conf" <> help "The configuration file to use."))
        (progDesc "Start the Labmap web server.") )

main :: IO ()
main = join $ execParser opts

data LabmapConf = LabmapConf
  { sshOpts :: [ String ]
  , machines :: Machines
  , outputFile :: FilePath
  , openingHour :: Int
  , closingHour :: Int
  , logLevel :: Priority
  , scanThreads :: Int
  , usersCacheHours :: Int
  , port :: Int
  } deriving (Read, Show)

instance FromJSON LabmapConf where
  parseJSON (Object conf)
    = LabmapConf <$>
        (words <$> (conf .: "sshOpts")) <*>
        (H.toList <$> (conf .: "machines")) <*>
        conf .: "outputFile" <*>
        conf .: "openingHour" <*>
        conf .: "closingHour" <*>
        (read <$> (conf .: "logLevel")) <*>
        conf .: "scanThreads" <*>
        conf .: "usersCacheHours" <*>
        conf .: "port"

userToJSON :: User -> Value
userToJSON u = object
    [ "username" .= username u
    , "fullName" .= fullName u
    , "photo" .= photo u
    , "groups" .= groups u
    ]

findSelf :: IO FilePath
findSelf = readSymbolicLink "/proc/self/exe"

sleepTime :: Int -> Int -> IO (Maybe NominalDiffTime)
sleepTime open close = do
  now@(ZonedTime (LocalTime day (TimeOfDay h _ _)) tz) <- getZonedTime
  return $ do
    wakeDay <- if
      | h < open -> Just day
      | h >= close -> Just (addDays 1 day)
      | otherwise -> Nothing
    let wakeTime = ZonedTime (LocalTime wakeDay (TimeOfDay open 0 0)) tz
    return $ zonedTimeToUTC wakeTime `diffUTCTime` zonedTimeToUTC now


getUserCommand :: IO ()
getUserCommand = getUser >>= print

type LabState = Either Text (M.Map Text Value)

scanForever :: LabmapConf -> Cached Users -> MVar LabState -> IO ()
scanForever LabmapConf{..} users labState = do
  resultChan <- newChan
  self <- findSelf
  noticeM "labmap" "Starting scan."
  scan sshOpts machines [ self, "getuser" ] resultChan scanThreads
  forever $ do
    sleepTime openingHour closingHour >>= \case
      Just s -> do
        infoM "labmap" ("Sleeping for " <> show s)
        putMVar labState (Left "CLOSED")
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
          Just userInfo -> userToJSON userInfo
    debugM "labmap" (T.unpack m <> ": " <> show s)
    modifyMVar_ labState $ \ls -> return $ Right $ case ls of
      Left _ -> M.singleton m s'
      Right state -> M.insert m s' state

serve :: Int -> MVar LabState -> IO ()
serve port labState = do
  noticeM "labmap" "Starting server."
  scotty port $ do
    get "/labState" $ do
      s <- liftIO $ readMVar labState
      S.json $ case s of
        Left m -> object ["unavailable" .= m]
        Right state -> toJSON state


serverCommand :: String -> IO ()
serverCommand configFile = do
  m'conf <- Yaml.decodeFile configFile
  case m'conf of
    Nothing -> putStrLn "Could not read configuration file"
    Just conf@LabmapConf{..} -> do
      updateGlobalLogger "labmap" (setLevel logLevel)
      users <- cache (hours usersCacheHours) getAllUsers
      labState <- newMVar (Right $ M.empty)
      forkIO $ scanForever conf users labState
      serve port labState