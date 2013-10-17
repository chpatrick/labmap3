module Labmap.Users where

import Labmap.Unix
import Labmap.Photo

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text, pack)
import System.Posix.User

data User = User
  { username :: Text
  , fullName :: Text
  , photo :: Maybe Text
  , groups :: [ Text ]
  } deriving Show

getAllUsers :: IO (M.Map Text User)
getAllUsers = do
  ps <- getPhotos
  ( gm, gn ) <- getGroupInfo
  pw <- getPasswd
  return $ M.mapMaybeWithKey (\un ui -> do
    let fullName = T.takeWhile (/=',') $ T.pack $ userGecos ui -- GECOS also has extra info
    let photo = M.lookup un ps
    primGroup <- M.lookup (userGroupID ui) gn
    extraGroups <- M.lookup un gm
    let groups = primGroup : extraGroups
    return $ User un fullName photo groups) pw
