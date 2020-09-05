#!/usr/bin/env stack
{- stack script --optimize --resolver lts-16.12  -}

import Control.Exception (try, SomeException)
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)


main :: IO ()
main = do
  args <- getArgs
  withSystemTempDirectory "asciinema" (createRec args)
  where
    createRec args dir = do
      callProcess "asciinema" asciinemaArgs
      callProcess "asciicast2gif" [recording, gif]
      try (removeFile mp4) :: (IO (Either SomeException ()))
      callProcess "ffmpeg"
        [ "-i", gif
        , "-movflags", "faststart"
        , "-pix_fmt", "yuv420p"
        , "-vf", "scale=trunc(iw/2)*2:trunc(ih/2)*2"
        , mp4 ]
      callProcess "xclip"
        [ "-sel", "c"
        , "-t", "video/mp4"
        , mp4 ]
      putStrLn mp4 
      where
        asciinemaArgs = case args of
          []        -> ["rec", recording] 
          [command] -> ["rec", "-c", command, recording]
        recording = dir <> "asciicast.rec"
        gif = dir <> "asciicast.gif"
        mp4 = "/tmp/asciicast.mp4"
