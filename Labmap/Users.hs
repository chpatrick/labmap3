module Labmap.Users(getAllUsers, User(..), Users) where

import Labmap.Unix
import Labmap.Photo

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import System.Posix.User (UserEntry(..))

data User = User
  { username :: Text
  , fullName :: Text
  , photo :: Maybe Text
  , groups :: [ Text ]
  } deriving Show

type Users = M.Map Text User

getAllUsers :: IO Users
getAllUsers = do
  photos <- getPhotos
  ( groupMembers, groupNames ) <- getGroupInfo
  passwd <- getPasswd
  -- Join the passwd database with everything else, if possible.
  let
    joinData username_ pwInfo = do
      let fullName_ = T.takeWhile (/=',') $ T.pack $ userGecos pwInfo -- GECOS also has extra info
      let photo_ = M.lookup username_ photos
      primGroup <- M.lookup (userGroupID pwInfo) groupNames
      extraGroups <- M.lookup username_ groupMembers
      let groups_ = primGroup : extraGroups
      return $ User username_ fullName_ photo_ groups_
  return $ M.mapMaybeWithKey joinData passwd
    
