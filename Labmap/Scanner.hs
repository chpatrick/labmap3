module Labmap.Scanner where

import Control.Monad
import Control.Concurrent
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit
import System.Log.Logger
import System.Process
import Text.Printf
import Text.Read

import Labmap.Common
import Labmap.Util

type Hostname = String

type Machines = [ ( Hostname, Int ) ]

enumerateMachines :: String -> Int -> [ String ]
enumerateMachines group count
  = map (printf "%s%02d" group) [1..count]

createWorkQueue :: Machines -> IO (LazyChan String)
createWorkQueue machines
  = newMVar $ cycle $ concatMap (uncurry enumerateMachines) machines

scanMachine :: [ String ] -> [ String ] -> String -> IO (Maybe MachineState)
scanMachine sshOpts cmd hostname = do
  let args = sshOpts ++ [ hostname ] ++ cmd
  debugM "labmap" $ unwords ("scan command:" : "ssh" : args)
  ( exitCode, result, _ ) <- readProcessWithExitCode "ssh" args ""
  return (guard (exitCode == ExitSuccess) >> readMaybe result)

scanner :: [ String ] -> [ String ] -> MVar () -> LazyChan Hostname -> Chan ( Text, Maybe MachineState ) -> IO ()
scanner opts cmd runVar work result = forever $ do
  debugM "labmap" "Getting work token..."
  readMVar runVar -- get a work token - the main thread can withdraw this if the scanners should be paused
  debugM "labmap" "Got work token."
  hostname <- readLazyChan work
  state <- scanMachine opts cmd hostname
  writeChan result ( T.pack hostname, state )

scan :: [ String ] -> Machines -> [ String ] -> MVar () -> Chan ( Text, Maybe MachineState ) -> Int -> IO [ ThreadId ]
scan opts machines cmd runVar results threads = do
  wq <- createWorkQueue machines
  replicateM threads $ forkIO (scanner opts cmd runVar wq results)
