module Labmap.Common(MachineState(..)) where

import Data.Text (Text)

data MachineState = Available | Occupied Text
  deriving ( Read, Show, Eq, Ord )
