{-# LANGUAGE NamedFieldPuns #-}

module Labmap.Util where

import Control.Concurrent.MVar
import Data.Time

type LazyChan a = MVar [ a ]

readLazyChan :: LazyChan a -> IO a
readLazyChan c = do
  x : xs <- takeMVar c
  putMVar c xs
  return x

data Cached a = Cached
  { action :: IO a
  , value :: MVar ( a, UTCTime )
  , lifespan :: NominalDiffTime
  }

cache :: NominalDiffTime -> IO a -> IO (Cached a)
cache lifespan action = do
  val <- action
  now <- getCurrentTime
  value <- newMVar ( val, now )
  return Cached
    { action = action
    , value = value
    , lifespan = lifespan
    }

getCached :: Cached a -> IO a
getCached (Cached {action, value, lifespan}) = do
  ( val, timestamp ) <- takeMVar value
  now <- getCurrentTime
  if now `diffUTCTime` timestamp > lifespan
    then do
      val' <- action
      putMVar value ( val', now )
      return val'
    else do
      putMVar value ( val, timestamp )
      return val

seconds :: Int -> NominalDiffTime
seconds = fromIntegral

minutes :: Int -> NominalDiffTime
minutes = seconds . (*60)

hours :: Int -> NominalDiffTime
hours = minutes . (*60)
