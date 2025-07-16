{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import System.FilePath (normalise)
import Turtle

parser :: Parser FilePath
parser = argPath "basedir"  "Base directory to check"

shellToList :: Fold a [a]
shellToList = Fold (\ys y -> ys++[y]) [] id

getMds :: IO [FilePath]
getMds = reduce shellToList (normalise <$> find (ends ".md") ".")

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  exists <- testdir dir
  if exists
  then reduce shellToList (normalise <$> ls dir)
  else pure []

getFilesSvg :: FilePath -> IO [FilePath]
getFilesSvg dir = fmap changeExt <$> getFiles dir
  where changeExt file = dropExtension file <.> "svg"

main :: IO ()
main = do
  basedir <- options "Check images in markdown files" parser
  cd basedir

  getMds >>= mapM_ print
  getFiles "files" >>= mapM_ print
  getFilesSvg "dot" >>= mapM_ print
  getFilesSvg "tex" >>= mapM_ print
  getFilesSvg "uml" >>= mapM_ print

