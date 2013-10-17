module Labmap.Scanner where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit
import System.Posix.Files
import System.Process
import Text.Printf
import Text.Read
import Text.Regex.PCRE

import Labmap.Common
import Labmap.Util

type SSHOpts = [ ( String, String ) ]

type Hostname = String

type Machines = [ ( Hostname, Int ) ]

-- TEMP HACKS

opts :: SSHOpts
opts =
  [ ( "StrictHostKeyChecking", "no" )
  , ( "UserKnownHostsFile", "/dev/null" )
  , ( "PreferredAuthentications", "gssapi-with-mic" )
  ]

machines :: Machines
machines
  = [ ( "pixel", 37 )
    , ( "texel", 38 )
    , ( "matrix", 23 )
    , ( "visual", 23 )
    , ( "corona", 42 )
    , ( "edge", 28 )
    ]

----

enumerateMachines :: String -> Int -> [ String ]
enumerateMachines group count
  = map (printf "%s%02d" group) [1..count]

createWorkQueue :: Machines -> IO (LazyChan String)
createWorkQueue machines
  = newMVar $ cycle $ concatMap (uncurry enumerateMachines) machines

scanMachine :: SSHOpts -> [ String ] -> String -> IO (Maybe MachineState)
scanMachine sshOpts cmd hostname = do
  let sshArgs = concatMap (\( key, val ) -> [ "-o", key ++ "=" ++ val ] ) sshOpts
  let args = (sshArgs ++ [ hostname ] ++ cmd)
  ( exitCode, result, err ) <- readProcessWithExitCode "ssh" args ""
  return (guard (exitCode == ExitSuccess) >> readMaybe result)

scanner :: SSHOpts -> [ String ] -> LazyChan Hostname -> Chan ( Text, Maybe MachineState ) -> IO ()
scanner opts cmd work result = forever $ do
  hostname <- readLazyChan work
  state <- scanMachine opts cmd hostname
  writeChan result ( T.pack hostname, state )

scan :: SSHOpts -> Machines -> [ String ] -> Chan ( Text, Maybe MachineState ) -> Int -> IO [ ThreadId ]
scan opts machines cmd results threads = do
  wq <- createWorkQueue machines
  replicateM threads $ forkIO (scanner opts cmd wq results)
