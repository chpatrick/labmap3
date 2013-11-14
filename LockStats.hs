{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BSL
import Data.Conduit
import Data.Char
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.Text as T
import Data.Text.Encoding
import Labmap.Lock
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Directory
import System.FilePath

data LockSession = LockSession
  { duration :: Int, samplingInterval :: Maybe Int }
    deriving Show

getLockFiles = do
  filenames <- getDirectoryContents screenLockDir
  return [ screenLockDir </> fn | fn <- filenames, notElem '.' fn ]

valid = CL.mapMaybe (\case Left _ -> Nothing; Right le -> Just le)

lockData fs = mapM_ CB.sourceFile fs $= CB.lines $= CL.map (parseLockEntry . decodeUtf8) $= valid

recent = CL.filter matchDay
  where
    matchDay LockEntry { lockTime = LocalTime date t }
      = let ( y, m, d ) = toGregorian date in y == 2013

labMachines = CL.filter (T.any isDigit . lockHostname)

groupSessions = CL.groupBy key
  where
    utcTime l = zonedTimeToUTC (ZonedTime (lockTime l) utc)
    key l l'
      = lockUser l == lockUser l' && abs (utcTime l' `diffUTCTime` utcTime l) < 70

{-
collectSessions = CL.map collect
  where
    collect les = LockSession dur interval
      where
        dur = lockDuration $ last les
        interval = case les of
          le : le' : _ -> Just (lockDuration le' - lockDuration le)
          _ -> Nothing

printSessions = CL.map (\LockSession { duration = dur } -> BSL.pack (show dur ++ " " ++ show (dur + 30) ++ "\n"))
-}

data LogNormal = LogNormal { meanLog :: Double, sdLog :: Double }
  deriving Show

instance Distribution LogNormal where
  cumulative ln x = cumulative standard ((log x - meanLog ln) / sdLog ln)

lnDist = LogNormal 4.01311 1.46509

lockProb :: Distribution d => d -> Double -> Double -> Double
lockProb dist lockTime lockDuration
  | raw > 0.95 = raw
  | otherwise = raw - abs (0.9 - raw) + 0.05 - sin ((raw - 0.71) * pi / 0.29) * 0.05
    where
      raw = complCumulative dist lockTime / complCumulative dist lockDuration

checkSessions dist = CL.concatMap check
  where
    check (l : ls) = positive ++ next ls
      where
        dur = fromIntegral $ lockDuration l
        positive = guard (dur > 30) >> return ( lockProb dist dur (dur - 30), True )
        next [] = [ ( lockProb dist (dur + 30) dur, False ) ]
        next ls = check ls

collectPredictions = CL.fold collect M.empty
  where
    buckets = 100 :: Int
    collect cs ( p, t ) = M.insertWith add (floor (p * fromIntegral buckets) * 100 `div` buckets) d cs
      where
        add ( x, y ) ( x', y' ) = ( x + x', y + y' )
        d = if t then ( 1 :: Int, 1 :: Int ) else ( 0, 1 )

main = do
  lockFiles <- getLockFiles
  predMap <- runResourceT (lockData lockFiles $= recent $= labMachines $= groupSessions $= checkSessions lnDist $$ collectPredictions)
  mapM_ (\(p, a) -> putStrLn (show p ++ " " ++ show a)) $ M.toList $ M.map (\(l, t) -> fromIntegral l / fromIntegral t * 100) predMap
