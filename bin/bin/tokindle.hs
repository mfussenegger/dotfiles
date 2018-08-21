#!/usr/bin/env stack
{- stack runghc --resolver lts-12.7 --package Unixutils --package split -}

import           Control.Exception     (finally)
import           Data.Char             (isSpace)
import           Data.List.Split       (splitOn)
import           Data.Semigroup        ((<>))
import           System.Directory      (removeFile)
import           System.Environment    (getArgs)
import           System.Process        (callProcess, readProcess)
import           System.Unix.Directory (withTemporaryDirectory)


mailFile :: String -> String -> IO ()
mailFile file title = do
  recipient <- takeWhile (not . isSpace) <$>
    readProcess "khard" ["email", "-p", "--remove-first-line", "kindle"] ""
  callProcess "mutt" ["-a", file, "-s", title, "--", recipient]


sendPageToKindle :: String -> String -> IO ()
sendPageToKindle url tmpDir = do
    let
      ext = last $ splitOn "." url
    case ext of
      "pdf" -> processPdf
      _     -> processPage
    where
      title = head $ splitOn "?" $ last (filter (not . null) (splitOn "/" url))
      epub = tmpDir <> title <> ".epub"
      mobi = tmpDir <> title <> ".mobi"
      pdf = tmpDir <> title <> ".pdf"
      pdfProcessed = tmpDir <> title <> "k2opt.pdf"
      processPage = do
        callProcess "pandoc" [url, "-t", "epub", "--output", epub]
        callProcess "ebook-convert" [epub, mobi]
        mailFile mobi title `finally` mapM_ removeFile [epub, mobi]
      processPdf = do
        callProcess "curl" [url, "--output", pdf]
        callProcess "k2pdfopt"
          [ "-dev", "kp3"
          , "-vls"
          , "-3"
          , "-o", pdfProcessed
          , "-vb", "1.25"
          , pdf ]
        mailFile pdfProcessed title `finally` mapM_ removeFile [pdf, pdfProcessed]


main :: IO ()
main = do
  [url] <- getArgs
  withTemporaryDirectory "kindle" (sendPageToKindle url)
