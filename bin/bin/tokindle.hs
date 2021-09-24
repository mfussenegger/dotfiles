#!/usr/bin/env stack
-- stack script --resolver lts-18.10 --package "process directory temporary split"

import           Control.Monad      (void)
import           Data.Char          (isSpace)
import           Data.List.Split    (splitOn)
import           Data.Semigroup     ((<>))
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)
import           System.IO.Temp     (withSystemTempDirectory)
import           System.Process     (callProcess, readProcess)


mailFile :: String -> String -> IO ()
mailFile file title = do
  recipient <- takeWhile (not . isSpace) <$>
    readProcess "khard" ["email", "-p", "--remove-first-line", "kindle"] ""
  let
    muttArgs = ["-a", file, "-s", title, "--", recipient]
  void $ readProcess "mutt" muttArgs "Have fun reading"


k2pdfopt :: String -> String -> IO ()
k2pdfopt infile outfile = callProcess "k2pdfopt"
  [ "-dev", "ko2"
  , "-vls"
  , "-3"
  , "-o", outfile
  , "-vb", "1.1"
  , infile ]


sendPageToKindle :: String -> String -> IO ()
sendPageToKindle url tmpDir = do
    let
      ext = last $ splitOn "." url
    case ext of
      "epub" -> processEpub
      "pdf"  -> processPdf
      _      -> processPage
    where
      title = head $ splitOn "?" $ last (filter (not . null) (splitOn "/" url))
      epub = tmpDir <> title <> ".epub"
      mobi = tmpDir <> title <> ".mobi"
      pdf = tmpDir <> title <> ".pdf"
      pdfProcessed = tmpDir <> title <> "k2opt.pdf"
      processPage = do
        callProcess "pandoc" [url, "-t", "epub", "--output", epub]
        callProcess "ebook-convert" [epub, mobi]
        mailFile mobi title
      processEpub = do
        callProcess "curl" [url, "--output", epub]
        callProcess "ebook-convert" [epub, mobi]
        mailFile mobi title
      processPdf = do
        fileExists <- doesFileExist url
        if fileExists
          then do
            k2pdfopt url pdfProcessed
            mailFile pdfProcessed title
          else do
            callProcess "curl" [url, "--output", pdf]
            k2pdfopt pdf pdfProcessed
            mailFile pdfProcessed title


main :: IO ()
main = do
  [url] <- getArgs
  withSystemTempDirectory "kindle" (sendPageToKindle url)
