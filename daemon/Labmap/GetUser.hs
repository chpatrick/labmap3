module Labmap.GetUser where

import Labmap.Common

import Data.List
import qualified Data.Text as T
import System.Utmp

-- Hardcoded for now, could be in config later.
ttys :: [ String ]
ttys = [ "tty7", "tty8", "tty9" ]

getUser :: IO MachineState
getUser = do
  us <- getutents
  return $ case find (\u -> utmpType u == UserProcess && line u `elem` ttys) us of
    Just u -> Occupied (T.pack (user u))
    Nothing -> Available
