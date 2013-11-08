{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Labmap.Lock(LockEntry(..), lastEntryForMachine, parseLockEntry, screenLockDir) where

import Control.Applicative
import Control.Monad.Instances()
import qualified Data.Text as T
import Data.Text(Text)
import qualified Data.Text.IO as TIO
import Data.Text.Read
import Data.Time
import System.FilePath
import System.IO
import System.IO.Error
import System.Locale

data LockEntry = LockEntry
  { lockTime :: LocalTime
  , lockHostname :: Text
  , lockUser :: Text
  , lockDuration :: Int
  } deriving ( Show, Ord, Eq )

parseLockEntry :: Text -> Either String LockEntry
parseLockEntry e = do
  let [ t, h, u, _, d ] = T.splitOn "," e
  t' <- case parseTime defaultTimeLocale "%a %b %e %H:%M:%S %Y" (T.unpack t) of
  	Nothing -> Left "Failed to parse date."
  	Just t' -> Right t'
  ( d', "" ) <- decimal d
  return $ LockEntry t' h u d'

screenLockDir :: FilePath
screenLockDir = "/vol/linux/autofiles/scrlock"

getLastLine :: FilePath -> IO (Either IOError Text)
getLastLine f = tryIOError $ withFile f ReadMode $ \h -> do
  hSeek h SeekFromEnd (-70)
  TIO.hGetLine h
  TIO.hGetLine h

lastEntryForMachine :: Text -> IO (Either String LockEntry)
lastEntryForMachine m
  = parse <$> getLastLine (screenLockDir </> T.unpack m)
    where
      parse (Left ioe) = Left (show ioe)
      parse (Right l) = parseLockEntry l
