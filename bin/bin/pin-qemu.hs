#!/usr/bin/env stack
{- stack script --optimize --resolver lts-23.1
    --package process
    --package parsec
-}

{-# LANGUAGE NamedFieldPuns #-}

import qualified System.Process as P
import qualified Data.List as L
import Text.Parsec
import Text.Parsec.Char
import Data.Foldable (traverse_)
import Control.Monad (when)


pid :: IO Int
pid = read <$> P.readProcess "pgrep" ["qemu-system-x86"] ""


data Thread = Thread
  { parent :: Int,
    child :: Int,
    cmd :: String
  } deriving (Show, Eq, Ord)


type Parser = Parsec String ()


threadLine :: Parser Thread
threadLine = do
  --  198151  198151 pts/6    00:00:38 qemu-system-x86
  skipWhitespace
  parent <- read <$> many1 digit
  skipWhitespace
  child <- read <$> many1 digit
  skipWhitespace
  skipText
  skipWhitespace
  skipText
  skipWhitespace
  cmd <- many1 (noneOf " \n")
  pure $ Thread { parent, child, cmd }
  where
    skipWhitespace = skipMany (char ' ')
    skipText = skipMany1 (noneOf " ")


threads :: Int -> IO (Either ParseError [Thread])
threads pid =
  parse psOutput "" <$> P.readProcess "ps" ["-T", "-p", show pid] ""
  where
    psOutput :: Parser [Thread]
    psOutput = 
      manyTill anyChar endOfLine
      >> threadLine `sepEndBy` endOfLine


pinThread :: (Int, Thread) -> IO ()
pinThread (num, thread) =
  when (parent thread /= child thread) $ do
    P.callProcess "taskset" ["-pc", show num, show $ child thread]


main :: IO ()
main = do
  threads' <- pid >>= threads
  case threads' of
    Left err -> print err
    Right threads'' -> do
      let qemuThreads = filter ((== "qemu-system-x86") . cmd) threads''
          sorted = L.sortOn cmd qemuThreads
          numbered = zip [1 ..] sorted
      traverse_ pinThread numbered
