#!/usr/bin/env stack
{- stack script --optimize --resolver lts-22.23
  --package network-uri
  --package directory
  --package process
-}

import System.Environment (getArgs)
import qualified System.Directory as D
import qualified Network.URI as URI
import qualified System.Process as P
import Control.Monad (unless)


createAndPrint :: String -> IO ()
createAndPrint ('/' : tail) = do
    home <- D.getHomeDirectory
    let userPath = home <> "/dev/" <> user <> "/"
        repoPath = userPath <> repo
    D.createDirectoryIfMissing True userPath
    doesExist <- D.doesDirectoryExist repoPath
    unless doesExist (cloneRepo userPath repoIdent)
    putStrLn repoPath
  where
    user = takeWhile (/= '/') tail
    repo = takeWhile (/= '/') . drop 1 . dropWhile (/= '/') $ tail
    repoIdent = user <> "/" <> repo
createAndPrint path = putStrLn $ "Invalid path: " <> path


cloneRepo :: FilePath -> String -> IO ()
cloneRepo dir repoIdent = do
    P.readCreateProcessWithExitCode proc { P.cwd = Just dir } ""
    pure ()
  where
    repo = "gh:" <> repoIdent
    proc = P.proc "git" ["clone", repo]


withUri :: URI.URI -> IO ()
withUri uri =
  if scheme == "http:" || scheme == "https:" && isGithubURI
    then createAndPrint $ URI.uriPath uri
    else putStrLn "No Github URI, nothing to do"
  where
    scheme = URI.uriScheme uri
    isGithubURI = (URI.uriRegName <$> URI.uriAuthority uri) == Just "github.com"


main :: IO ()
main = do
  [url] <- getArgs
  let uri = URI.parseAbsoluteURI url
  case uri of
    Nothing -> pure ()
    Just uri' -> withUri uri'
