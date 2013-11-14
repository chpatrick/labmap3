module Labmap.Scanner where

import Control.Monad
import Control.Concurrent
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit
import System.Log.Logger
import System.Process
import Text.Printf

import Labmap.Common
import Labmap.Util

type Hostname = String

type Machines = [ ( Hostname, Int ) ]

enumerateMachines :: String -> Int -> [ String ]
enumerateMachines family count
  = map (printf "%s%02d" family) [1..count]

createWorkQueue :: Machines -> IO (LazyChan String)
createWorkQueue machines
  = newMVar $ cycle $ concatMap (uncurry enumerateMachines) machines

parseWho :: String -> Maybe [ ( String, String ) ]
parseWho w = forM (lines w) $ \l -> do
  let ( username : tty : _ ) = words l
  return ( username, tty )

scanMachine :: [ String ] -> String -> IO (Maybe MachineState)
scanMachine sshOpts hostname = do
  let args = sshOpts ++ [ hostname, "who" ]
  debugM "labmap" $ unwords ("scan command:" : "ssh" : args)
  ( exitCode, result, _ ) <- readProcessWithExitCode "ssh" args ""
  return $ do
    guard (exitCode == ExitSuccess)
    us <- parseWho result
    return $ case find (\(_, tty) -> tty `elem` [ "tty7", "tty8", "tty9" ]) us of
      Nothing -> Available
      Just ( u, _ ) -> Occupied $ T.pack u

scanner :: [ String ] -> MVar () -> LazyChan Hostname -> Chan ( Text, Maybe MachineState ) -> IO ()
scanner opts runVar work result = forever $ do
  debugM "labmap" "Getting work token..."
  readMVar runVar -- get a work token - the main thread can withdraw this if the scanners should be paused
  debugM "labmap" "Got work token."
  hostname <- readLazyChan work
  state <- scanMachine opts hostname
  writeChan result ( T.pack hostname, state )

scan :: [ String ] -> Machines -> MVar () -> Chan ( Text, Maybe MachineState ) -> Int -> IO [ ThreadId ]
scan opts machines runVar results threads = do
  wq <- createWorkQueue machines
  replicateM threads $ forkIO (scanner opts runVar wq results)
