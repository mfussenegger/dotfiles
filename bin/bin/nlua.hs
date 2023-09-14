#!/usr/bin/env stack
{- stack script --optimize --resolver lts-21.11 -}

import System.Environment (getArgs)
import System.Process (spawnProcess, waitForProcess, CreateProcess (..))
import System.Exit (exitWith)


main :: IO ()
main = do
  args <- remap <$> getArgs
  spawnProcess "v" ("-Es" : args) >>= waitForProcess >>= exitWith


--- >>> remap ["foo.lua"]
-- ["-l","foo.lua"]
---
--- >>> remap ["-e", "stat"]
-- ["-c","lua stat"]
---
--- >>> remap ["-e", "stat", "luascript_path"]
-- ["-c","lua stat","-l","luascript_path"]
---
--- >>> remap ["-e", "stat", "-l", "luascript_path"]
-- ["-c","lua stat","-l","luascript_path"]
remap :: [String] -> [String]
remap [] = []
remap [x] = ["-l", x]
remap ("-e" : x : xs)
  | null xs = newHead
  | head xs == "-l" = newHead <> xs
  | otherwise = newHead <> ("-l" : xs)
  where
    newHead = ["-c", "lua " <> x]
remap (x : xs) = x : remap xs
