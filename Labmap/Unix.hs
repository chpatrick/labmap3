{-# LANGUAGE NamedFieldPuns #-}

module Labmap.Unix where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import System.Posix.User
import System.Posix.Types

getGroupInfo :: IO ( M.Map Text [ Text ], M.Map GroupID Text )
getGroupInfo = do
  ge <- getAllGroupEntries
  let memberships = do
	GroupEntry { groupName, groupMembers } <- ge
	user <- groupMembers
	return ( T.pack user, [ T.pack groupName ] )
  let groupNames = map (\g -> ( groupID g, T.pack (groupName g) )) ge
  return ( M.fromListWith (++) memberships, M.fromList groupNames )

getPasswd :: IO (M.Map Text UserEntry)
getPasswd = do
  es <- getAllUserEntries
  let us = map (\e -> ( T.pack (userName e), e )) es
  return $ M.fromList us
