{-# LANGUAGE ForeignFunctionInterface #-}

#include "utmp.h"

module System.Utmp(UtmpType(..), Utmp(..), getutents) where

import Control.Applicative
import Data.Int
import Foreign
import Foreign.C.String

data UtmpType
  = Empty
  | RunLevel
  | BootTime
  | NewTime
  | OldTime
  | InitProcess
  | LoginProcess
  | UserProcess
  | DeadProcess
  | Accounting
    deriving ( Eq, Enum, Show )

toUtmpType :: Word16 -> UtmpType
toUtmpType = toEnum . fromIntegral

data Utmp = Utmp
  { utmpType :: UtmpType
  , line :: String
  , user :: String
  } deriving Show

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Utmp where
  alignment _ = #{alignment struct utmp}
  sizeOf _ = #{size struct utmp}
  peek p = Utmp <$>
    (toUtmpType <$> #{peek struct utmp, ut_type} p) <*>
    peekCString (#{ptr struct utmp, ut_line} p) <*>
    peekCString (#{ptr struct utmp, ut_user} p)

foreign import ccall unsafe "utmp.h setutent"
  setutent :: IO ()

foreign import ccall unsafe "utmp.h endutent"
  endutent :: IO ()

foreign import ccall unsafe "utmp.h getutent"
  getutent :: IO (Ptr Utmp)

getutents :: IO [ Utmp ]
getutents = do
  setutent
  let getutents' = do
      up <- getutent
      if up == nullPtr then return [] else (:) <$> peek up <*> getutents'
  us <- getutents'
  endutent
  return us
