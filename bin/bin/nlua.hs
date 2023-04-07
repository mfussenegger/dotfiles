#!/usr/bin/env stack
{- stack script --optimize --resolver lts-20.17 -}

import System.Environment (getArgs)
import System.Process (createProcess_, proc, spawnProcess, waitForProcess, CreateProcess (..), StdStream (..))
import System.Exit (exitWith)


main :: IO ()
main = do
  args <- remap <$> getArgs
  --(stdin, stdout, stderr, handle) <- createProcess_ "nlua" (proc "v" ("-Es" : args)) { std_in = Inherit }
  handle <- spawnProcess "v" ("-Es" : args)
  waitForProcess handle >>= exitWith


--- >>> remap ["-e", "stat"]
-- ["-c","lua stat"]
--
--- >>> remap ["-e", "stat", "luascript_path"]
-- ["-c","lua stat","-l","luascript_path"]
--
--- >>> remap ["-e", "stat", "-l", "luascript_path"]
-- ["-c","lua stat","-l","luascript_path"]
remap :: [String] -> [String]
remap [] = []
remap ("-e" : x : xs)
  | null xs = newHead
  | head xs == "-l" = newHead <> xs
  | otherwise = newHead <> ("-l" : xs)
  where
    newHead = ["-c", "lua " <> x]
remap (x : xs) = x : remap xs
