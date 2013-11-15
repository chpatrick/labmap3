{-# LANGUAGE NamedFieldPuns #-}

module Labmap.Users(getUsersAndGroupCounts, User(..), Users, GroupCounts) where

import Labmap.Photo
import Labmap.Util

import Control.Applicative
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import System.Posix.User
import System.Posix.Types

data User = User
  { username :: Text
  , fullName :: Text
  , photo :: Maybe Text
  , groups :: [ Text ]
  } deriving Show

type Users = M.Map Text User
type GroupCounts = M.Map Text Int

secondaryMemberships :: [ GroupEntry ] -> M.Map GroupID [ Text ]
secondaryMemberships gs = M.fromList [ ( groupID, map T.pack groupMembers ) | GroupEntry { groupID, groupMembers } <- gs ]

memberships :: [ UserEntry ] -> [ GroupEntry ] -> M.Map GroupID [ Text ]
memberships us gs
  = foldl addUser (secondaryMemberships gs) us
    where
      addUser gs' UserEntry { userName, userGroupID }
        = M.adjust ((T.pack userName):) userGroupID gs'

groupNames :: [ GroupEntry ] -> M.Map GroupID Text
groupNames = M.fromList . map (\GroupEntry { groupID, groupName } -> ( groupID, T.pack groupName ))

getUsersAndGroupCounts :: IO ( Users, GroupCounts )
getUsersAndGroupCounts = do
  us <- getAllUserEntries
  gs <- getAllGroupEntries
  ps <- getPhotos
  let gns = groupNames gs
  let ms = memberships us gs
  let ms' = invert ms
  let
    makeUser UserEntry { userName, userGecos } = do
      let userName_ = T.pack userName
      let fullName_ = T.pack $ takeWhile (/=',') userGecos
      let photo_ = M.lookup userName_ ps
      groupIDs <- M.lookup userName_ ms'
      groups_ <- mapM (`M.lookup`gns) groupIDs
      return ( userName_, User userName_ fullName_ photo_ groups_ )
  let users = M.fromList $ mapMaybe makeUser us
  let
    makeGroupCount ( _, [] ) = Nothing
    makeGroupCount ( gid, mbs ) = (,) <$> M.lookup gid gns <*> pure (length mbs)
  let groupCounts = M.fromList $ mapMaybe makeGroupCount (M.assocs ms)
  return ( users, groupCounts )

{-
getPasswd :: IO (M.Map Text UserEntry)
getPasswd = do
  es <- getAllUserEntries
  let us = map (\e -> ( T.pack (userName e), e )) es
  return $ M.fromList us
-}

{-
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
-}    
