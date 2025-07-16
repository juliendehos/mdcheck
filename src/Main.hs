{-# LANGUAGE OverloadedStrings #-}

import CMark
import Control.Monad (forM_)
import Data.List (foldl')
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

main :: IO ()
main = do
  basedir <- options "Check images in markdown files" parser
  cd basedir

  mds <- getMds
  files <- getFiles "files"
  dots <- getFilesSvg "dot"
  tex <- getFilesSvg "tex"
  umls <- getFilesSvg "uml"

  forM_ mds $ \md -> do
    print md
    input <- T.readFile md
    let node = commonmarkToNode [] input
        imgs = nodeToImages node
    mapM_ putStrLn imgs

  
  pure ()

