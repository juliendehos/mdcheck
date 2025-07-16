{-# LANGUAGE OverloadedStrings #-}

import CMark
import Data.List (foldl', (\\))
import Data.Text qualified as T
import Data.Text.IO qualified as T
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

nodeToImages :: Node -> [FilePath]
nodeToImages = go []
  where
    go acc (Node _ (IMAGE url _) ns) = normalise (T.unpack url) : foldl' go acc ns
    go acc (Node _ _ ns) = foldl' go acc ns

imagesInFile :: FilePath -> IO [FilePath]
imagesInFile = fmap (nodeToImages . commonmarkToNode []) . T.readFile

main :: IO ()
main = do
  basedir <- options "Check images in markdown files" parser
  cd basedir

  mds <- getMds
  mdImages <- concat <$> traverse imagesInFile mds

  images <- concat <$> sequence 
    [ getFiles "files"
    , getFilesSvg "dot"
    , getFilesSvg "tex"
    , getFilesSvg "uml"
    ]

  putStrLn "\n*** not found ***"
  mapM_ putStrLn $ mdImages \\ images

  putStrLn "\n*** unused ***"
  mapM_ putStrLn $ images \\ mdImages

