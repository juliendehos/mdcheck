{-# LANGUAGE OverloadedStrings #-}

import CMark
import Control.Monad (forM_)
import Data.List (foldl', (\\), nub, sort)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prelude hiding (FilePath)
import System.FilePath (normalise)
import Turtle hiding (nub, sort)

parser :: Parser (Bool, FilePath)
parser = (,)
  <$> switch "unused" 'u' "Check unused images instead of missing images"
  <*> argPath "basedir" "Base directory to check"

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
imagesInFile = fmap (nub . nodeToImages . commonmarkToNode []) . T.readFile

main :: IO ()
main = do
  (unused, basedir) <- options "Check images in markdown files" parser
  cd basedir

  mds <- getMds
  mdImages <- traverse imagesInFile mds

  images <- nub . sort . concat <$> sequence 
    [ getFiles "images"
    , getFilesSvg "dot"
    , getFilesSvg "tikz"
    , getFilesSvg "uml"
    ]

  if unused
  then 
    mapM_ putStrLn $ images \\ concat mdImages
  else
    forM_ (zip mds mdImages) $ \(md, mdImage) -> 
      forM_ (mdImage \\ images) $ \img ->
        putStrLn $ md ++ " -> " ++ img

  -- print mdImages
  -- print images

