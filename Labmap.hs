{-# LANGUAGE OverloadedStrings, MultiWayIf, LambdaCase, RecordWildCards, TupleSections #-}

module Main where

import Labmap.Common
import Labmap.Conf
import Labmap.Lock
import Labmap.Scanner
import Labmap.Users
import Labmap.Util

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import qualified Data.Map.Strict as M
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static
import Options.Applicative
import System.Log.Logger
import Web.Scotty as S

opts :: ParserInfo (IO ())
opts = info (helper <*> args) (fullDesc <> header "Labmap 3.0")
  where
    args = serverCommand <$>
        strOption (short 'c' <> value "labmap.conf" <> help "The configuration file to use.")

main :: IO ()
main = join $ execParser opts

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


type LabState = Either Text (M.Map Text Value)

makeResult :: Cached ( Users, GroupCounts ) -> ( Text, Maybe MachineState ) -> IO Value
makeResult _ ( _, Nothing ) = return "UNKNOWN"
makeResult _ ( _, Just Available ) = return "AVAILABLE"
makeResult ugc ( m, Just (Occupied u) ) = do
  ( users, _ ) <- getCached ugc
  
  case M.lookup u users of
    Nothing -> return "UNKNOWN"
    Just ui -> do
      let resf = [ "username" .= u
               , "fullName" .= fullName ui
               , "photo" .= photo ui
               , "groups" .= groups ui
               ]
      e'le <- lastEntryForMachine m
      object <$> case e'le of
        Left e
          -> warningM "labmap" ("Failed to check lock status:" ++ e) >> return resf
        Right le | lockUser le == u -> do
          now@(ZonedTime _ tz) <- getZonedTime
          let lt = ZonedTime (lockTime le) tz
          let td = (zonedTimeToUTC now `diffUTCTime` zonedTimeToUTC lt) - fromIntegral (lockDuration le * 60)
          return $ if td < 30 * 60
            then resf ++
            [ "lockTime" .= lt
            , "lockDuration" .= lockDuration le
            ]
            else resf
        _ -> return resf

scanForever :: LabmapConf -> Cached ( Users, GroupCounts ) -> MVar LabState -> IO ()
scanForever LabmapConf{..} ugc labState = do
  resultChan <- newChan
  noticeM "labmap" "Starting scan."
  runVar <- newMVar ()
  scan sshOpts machines runVar resultChan scanThreads
  forever $ do
    sleepTime openingHour closingHour >>= \case
      Just s -> do
        let t = round (s * 1000000)
        infoM "labmap" ("Sleeping for " ++ show t ++ " microseconds")
        swapMVar labState (Left "CLOSED")
        -- confiscate the runvar while we sleep, this will block the scanner threads
        withMVar runVar $ const (threadDelay t)
        infoM "labmap" "Woke up"
      Nothing -> return ()
    ( m, s ) <- readChan resultChan
    s' <- makeResult ugc ( m, s )
    debugM "labmap" (T.unpack m <> ": " <> show s)
    modifyMVar_ labState $ \ls -> return $!! Right $ case ls of
      Left _ -> M.singleton m s'
      Right state -> M.insert m s' state

serve :: Int -> MVar LabState -> Cached ( Users, GroupCounts ) -> IO ()
serve port labState ugc = do
  noticeM "labmap" "Starting server."
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ do
      addHeader "Content-Type" "text/html"
      file "static/index.html"

    get "/labstate" $ do
      s <- liftIO $ readMVar labState
      S.json $ case s of
        Left m -> object ["UNAVAILABLE" .= m]
        Right state -> toJSON state

    get "/mygroups" $ do
      m'username <- reqHeader "X-Request-User"
      case m'username of
        Nothing -> do
          liftIO $ warningM "labmap" ("/mygroups: missing X-Request-User")
          status status400
        Just username -> do
          ( users, groupCounts ) <- liftIO (getCached ugc)
          case M.lookup (TL.toStrict username) users of
            Nothing -> do
              liftIO $ warningM "labmap" ("/mygroups: unknown user " ++ TL.unpack username)
              status status500
            Just user -> do
              S.json $ M.fromList $ mapMaybe (\g -> (g,) <$> M.lookup g groupCounts ) (groups user)

serverCommand :: String -> IO ()
serverCommand configFile = do
  m'conf <- loadConfig configFile
  case m'conf of
    Nothing -> putStrLn "Could not read configuration file"
    Just conf@LabmapConf{..} -> do
      updateGlobalLogger "labmap" (setLevel logLevel)
      ugc <- cache (hours usersCacheHours) getUsersAndGroupCounts
      labState <- newMVar (Right $ M.empty)
      forkIO $ scanForever conf ugc labState
      serve port labState ugc
