#!/usr/bin/env stack
{- stack runghc --package Unixutils --package split -}

import           Control.Exception     (finally)
import           Data.Char             (isSpace)
import           Data.List.Split       (splitOn)
import           Data.Semigroup        ((<>))
import           System.Directory      (removeFile)
import           System.Environment    (getArgs)
import           System.Process        (callProcess, readProcess)
import           System.Unix.Directory (withTemporaryDirectory)


main :: IO ()
main = do
  [url] <- getArgs
  withTemporaryDirectory "kindle" (sendPageToKindle url)
  where
    sendPageToKindle url tmpDir = do
      let
        title = head $ splitOn "?" $ last (filter (not . null) (splitOn "/" url))
        epub = tmpDir <> title <> ".epub"
        mobi = tmpDir <> title <> ".mobi"
      callProcess "pandoc" [url, "-t", "epub", "--output", epub]
      callProcess "ebook-convert" [epub, mobi]
      mailMobi mobi title `finally` removeFiles [epub, mobi]
    removeFiles = mapM_ removeFile
    mailMobi mobi title = do
      recipient <- takeWhile (not . isSpace) <$>
        readProcess "khard" ["email", "-p", "--remove-first-line", "kindle"] ""
      callProcess "mutt" ["-a", mobi, "-s", title, "--", recipient]
