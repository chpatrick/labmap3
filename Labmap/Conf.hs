{-# LANGUAGE OverloadedStrings #-}

module Labmap.Conf(LabmapConf(..), loadConfig) where

import Labmap.Scanner

import Control.Applicative
import Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Yaml as Yaml
import System.Log.Logger
import Text.Read

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
  , restrictLocal :: Bool
  } deriving (Read, Show)

instance FromJSON LabmapConf where
  parseJSON
    = withObject "LabmapConf" $ \conf -> LabmapConf <$>
	    (words <$> (conf .: "sshOpts")) <*>
	    (H.toList <$> (conf .: "machines")) <*>
	    conf .: "outputFile" <*>
	    conf .: "openingHour" <*>
	    conf .: "closingHour" <*>
	    (conf .: "logLevel" >>= parseRead) <*>
	    conf .: "scanThreads" <*>
	    conf .: "usersCacheHours" <*>
	    conf .: "port" <*>
	    conf .: "restrictLocal"

parseRead :: Read r => String -> Yaml.Parser r
parseRead t = case readMaybe t of
  Just v -> return v
  Nothing -> fail "Could not parse value."

loadConfig :: FilePath -> IO (Maybe LabmapConf)
loadConfig = Yaml.decodeFile
