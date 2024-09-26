#!/usr/bin/env stack
{- stack script --optimize --resolver lts-22.29
  --package network-uri
  --package directory
  --package process
-}

{-# LANGUAGE OverloadedRecordDot #-}

import System.Environment (getArgs)
import qualified System.Directory as D
import qualified Network.URI as URI
import qualified System.Process as P
import Control.Monad (unless)
import Data.Foldable (for_)


data ParsedPath = ParsedPath
  { user :: String,
    repo :: String
  }
  deriving (Eq, Show)


--- >>> parse "/mfussenegger/dotfiles/"
-- Just (ParsedPath {user = "mfussenegger", repo = "dotfiles"})
parse :: String -> Maybe ParsedPath
parse ('/' : tail) =
  let user = takeWhile (/= '/') tail
      repo = takeWhile (/= '/') . drop 1 . dropWhile (/= '/') $ tail
  in Just ParsedPath { user, repo }
parse _ = Nothing


createAndPrint :: String -> ParsedPath -> IO ()
createAndPrint prefix path = do
    home <- D.getHomeDirectory
    let userPath = home <> "/dev/" <> path.user <> "/"
        repoPath = userPath <> path.repo
    D.createDirectoryIfMissing True userPath
    doesExist <- D.doesDirectoryExist repoPath
    unless doesExist (cloneRepo prefix userPath repoIdent)
    putStrLn repoPath
  where
    repoIdent = path.user <> "/" <> path.repo


cloneRepo :: String -> FilePath -> String -> IO ()
cloneRepo prefix dir repoIdent = do
    P.readCreateProcessWithExitCode proc { P.cwd = Just dir } ""
    pure ()
  where
    repo = prefix <> repoIdent
    proc = P.proc "git" ["clone", repo]


withUri :: URI.URI -> IO ()
withUri uri =
  if scheme == "http:" || scheme == "https:"
    then case URI.uriRegName <$> URI.uriAuthority uri of
      Just "github.com" -> createAndPrint "gh:" parsedPath
      Just "codeberg.org" -> createAndPrint "cb:" parsedPath
      _ -> putStrLn "No Github URI, nothing to do"
    else putStrLn "No HTTP URI, nothing to do"
  where
    scheme = URI.uriScheme uri
    Just parsedPath = parse $ URI.uriPath uri


main :: IO ()
main = do
  [url] <- getArgs
  let uri = URI.parseAbsoluteURI url
  for_ uri withUri
