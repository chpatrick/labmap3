{-# LANGUAGE OverloadedStrings #-}

module Labmap.Photo(getPhotos) where

import Control.Applicative hiding (some, many, optional)
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import System.Directory
import System.FilePath
import Text.Parsec
import Text.Parsec.Text()

parseFilename :: FilePath -> Maybe Text
parseFilename fn
  = let parser = do
	name <- T.pack <$> many1 letter
	year <- T.pack <$> many digit
	string ".jp"
	optional (char 'e')
	char 'g'
	eof
	return (name <> year)
    in case parse parser "" (T.pack fn) of
      Left _ -> Nothing
      Right u -> Just u

scanDir :: FilePath -> IO [ ( Text, FilePath ) ]
scanDir dir = do
  cs <- getDirectoryContents dir
  return $ mapMaybe (\fn -> (\n -> ( n, fn ) ) <$> parseFilename fn) cs

staffDir :: FilePath
staffDir = "/vol/www/doc/people/staffphotos/photos"

baseStudentDir :: FilePath
baseStudentDir = "/vol/www/doc/internal/photosearch/pics"

getStudentDirs :: IO [ FilePath ]
getStudentDirs
  = filter (isPrefixOf "pics") <$> getDirectoryContents baseStudentDir

getPhotos :: IO (M.Map Text Text)
getPhotos = do
  staff <- map (fmap ("staff" </>)) <$> scanDir staffDir
  studentDirs <- getStudentDirs
  students <- forM studentDirs $ \dir ->
    map (fmap (combine ("students" </> dir))) <$> scanDir (baseStudentDir </> dir)  
  return $ M.fromList $ map (fmap T.pack) (staff ++ concat students)

