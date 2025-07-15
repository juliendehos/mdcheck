{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Turtle

parser :: Parser FilePath
parser = argPath "basedir"  "Base directory to check"

checkdir :: FilePath -> IO ()
checkdir dir = do
  exists <- testdir dir
  when exists $ do
    view (ls dir)

main :: IO ()
main = do
    basedir <- options "Check images in markdown files" parser
    cd basedir

    view (find (ends ".md") "." )

    view (ls "./files/" )

    checkdir "uml"
    checkdir "dot"
    checkdir "tex"

