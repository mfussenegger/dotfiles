#!/usr/bin/env stack
{- stack script --optimize --resolver lts-17.9 
 --package "aeson process bytestring regex-pcre text either utf8-string containers"
 --package "http-client http-client-tls directory unix"
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where


import Data.Aeson
  ( FromJSON,
    decode,
    eitherDecode
  )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.List (findIndex, isInfixOf)
import qualified Data.Map.Strict as M
import Data.Maybe
  ( catMaybes,
    fromJust,
    fromMaybe,
  )
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import System.Directory
  ( canonicalizePath,
    doesFileExist,
    getHomeDirectory, removeFile
  )
import System.Environment (getArgs)
import System.Posix.Files (rename)
import System.Process (callProcess, readProcess, callCommand)
import Text.Regex.PCRE ((=~))
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Either.Combinators (rightToMaybe)
import Text.Printf (printf)
import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)


data SwayOutput = SwayOutput
  { id :: Maybe Int,
    name :: T.Text,
    active :: Bool,
    focused :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON SwayOutput


class ToString a where
  str :: a -> String

instance ToString T.Text where
  str = T.unpack

instance ToString String where
  str = Prelude.id


getOutputs :: IO [SwayOutput]
getOutputs = do
  outputs <- readProcess "swaymsg" ["-r", "-t", "get_outputs"] ""
  let
    swayOutputs :: Either String [SwayOutput]
    swayOutputs = eitherDecode (BL.fromString outputs)
  case swayOutputs of
    Left err -> error $ "Couldn't decode output of swaymsg -t get_outputs:" <> err
    Right outputs' -> pure outputs'
  

selection :: [String] -> IO String
selection input = do
  args <- getArgs
  let
    runTool tool args = trim <$> readProcess tool args (unlines input)
    tool = case args of
      [] -> "bemenu"
      [arg] -> arg
  if tool == "bemenu"
    then do
      idx <- getDisplayIdx
      runTool "bemenu" ["-m", show idx, "-l", "30", "--fn", "JuliaMono 16"]
    else runTool tool []
  where
    getDisplayIdx :: IO Int
    getDisplayIdx = do
      outputs <- getOutputs
      pure $ fromMaybe 0 (findIndex (fromMaybe False . focused) . reverse $ outputs)


pickOne :: ToString b => [a] -> (a -> b) -> IO (Maybe a)
pickOne xs formatX = do
  selected <- selection formattedXs
  case T.splitOn " ¦ " (T.pack selected) of
    (idx : _) -> pure . fmap (xs !!) . rightToMaybe $ fst <$> T.decimal idx
    _         -> pure Nothing
  where
    formattedXs = fmap renderX (zip [(0 :: Int)..] xs)
    renderX (idx, x) = printf "%03d ¦ %s" idx (str $ formatX x)



trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


keyboard :: IO ()
keyboard = do
  xset ["r", "rate", "200", "40"]
  callProcess "setxkbmap" [
      "-layout", "de",
      "-variant", "nodeadkeys",
      "-option", "caps:escape"
      ]


barMode :: String -> IO ()
barMode mode = callProcess "swaymsg" (["bar", "mode"] ++ [mode])


xrandrOn :: IO ()
xrandrOn = do
  outputs <- getOutputs
  case filter (not . active) outputs of
    []       -> putStrLn "No inactive output"
    [output] -> activate output
    outputs' -> pickOne outputs' name >>= maybeActivate
  where
    maybeActivate (Just output) = activate output
    maybeActivate Nothing = pure ()
    activate :: SwayOutput -> IO ()
    activate output = callProcess "swaymsg" ["output", T.unpack $ name output, "enable"]


xrandrOff :: IO ()
xrandrOff = do
  outputs <- getOutputs
  case filter active outputs of
    []       -> putStrLn "No active output"
    [output] -> disable output
    outputs'  -> pickOne outputs' name >>= maybeDisable
  where
    disable :: SwayOutput -> IO ()
    disable output = callProcess "swaymsg" ["output", T.unpack $ name output, "disable"]
    maybeDisable :: Maybe SwayOutput -> IO ()
    maybeDisable (Just output) = disable output
    maybeDisable _ = pure ()


xset :: [String] -> IO ()
xset = callProcess "xset"


presOff :: IO ()
presOff = do
  callProcess "systemctl" ["--user", "start", "swayidle.service"]
  changeVimColorScheme ("tempus_totus", "gruvbox8_hard") ("light", "dark")
  alacrittyConfig <- expandUser "~/.config/alacritty/alacritty.yml" >>= canonicalizePath
  sed alacrittyConfig "colors: *light" "colors: *dark"
  sed alacrittyConfig "    family: JetBrains Mono" "    family: JetBrains Mono Light"


presOn :: IO ()
presOn = do
  callProcess "systemctl" ["--user", "stop", "swayidle.service"]
  changeVimColorScheme ("gruvbox8_hard", "tempus_totus") ("dark", "light")
  alacrittyConfig <- expandUser "~/.config/alacritty/alacritty.yml" >>= canonicalizePath
  sed alacrittyConfig "colors: *dark" "colors: *light"
  sed alacrittyConfig "    family: JetBrains Mono Light" "    family: JetBrains Mono"


sed :: FilePath -> String -> String -> IO ()
sed filepath target replacement =  do
  let
    tmpFile = filepath ++ ".tmp"
  contents <- lines <$> readFile filepath
  writeFile tmpFile (unlines (map (maybeReplace target replacement) contents))
  rename tmpFile filepath
  where
    maybeReplace target replacement line = if line == target then replacement else line


expandUser :: FilePath -> IO FilePath
expandUser ('~' : xs) = do
  homeDir <- getHomeDirectory
  pure $ homeDir ++ xs
expandUser xs = pure xs


changeVimColorScheme :: (String, String) -> (String, String) -> IO ()
changeVimColorScheme colorscheme background = do
  vimrc <- expandUser "~/.config/nvim/options.vim" >>= canonicalizePath
  sed vimrc ("colorscheme " ++ fst colorscheme) ("colorscheme " ++ snd colorscheme)
  sed vimrc ("set background=" ++ fst background) ("set background=" ++ snd background)
  instances <- lines <$> readProcess "nvr" ["--serverlist"] ""
  let changeColor = "<Esc>:set background=" ++ snd background ++ "<CR>:colorscheme " ++ snd colorscheme ++ "<CR>"
  for_ instances (\x ->
    callProcess "nvr" ["--servername", x, "--remote-send", changeColor])


callContacts :: IO ()
callContacts = do
  contactsPath <- expandUser "~/.config/dm/contacts.json"
  contents <- BL.readFile contactsPath
  let
    contacts = fromMaybe M.empty (decode contents :: Maybe (M.Map String String))
  choice <- selection $ M.keys contacts
  callProcess "xdg-open" [fromJust $ M.lookup choice contacts]


fromUriOrCache :: String -> String -> IO BL.ByteString
fromUriOrCache cachePath source = do
  resolvedPath <- expandUser cachePath
  fileExists <- doesFileExist resolvedPath
  if fileExists
    then BL.readFile resolvedPath
    else do
      manager <- HTTP.newManager HTTP.tlsManagerSettings
      req <- HTTP.parseRequest source
      content <- HTTP.responseBody <$> HTTP.httpLbs req manager
      BL.writeFile resolvedPath content
      pure content


data Emoji = Emoji
  { emoji :: String
  , description :: String }
  deriving (Show, Generic)


instance FromJSON Emoji


selectEmoji :: IO ()
selectEmoji = do
  emojiFileContents <- fromUriOrCache cachePath source
  let
    emojis = catMaybes <$> (eitherDecode emojiFileContents :: Either String [Maybe Emoji])
  case emojis of
    (Left err)-> error $ "Couldn't decode " <> cachePath <> ": " <> err
    (Right emojis') -> do
      selected <- pickOne emojis' (\e -> emoji e <> " " <> description e)
      case selected of
        Nothing -> return ()
        (Just selected') -> do
          let
            emoji' = emoji selected'
          putStrLn emoji'
          callProcess "wl-copy" [emoji']

  where
    source = "https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json"
    cachePath = "~/.config/dm/emoji.json"


selectSong :: IO ()
selectSong = do
  song <- showPlaylist >>= selection
  callProcess "mpc" ["play", songNr song]
  where
    showPlaylist = lines <$> readProcess "mpc" ["playlist", "-f", "%position% ¦ %artist% - %title%"] ""
    songNr = reverse . drop 1 . reverse . takeWhile (/= '¦')


data SinkInput = SinkInput
  { inputNumber :: Int,
    inputAppName :: T.Text
  }
  deriving (Eq, Show)


data Sink = Sink
  { sinkName :: T.Text,
    sinkDescription :: T.Text
  }
  deriving (Eq, Show)


-- > parseSinkInputs "Sink Input #135\nGarbage\n      application.name = \"Music Player Daemon\""
-- [ SinkInput
--    { inputNumber = 135
--    , inputAppName = ""Music Player Daemon""
--    }
-- ]
parseSinkInputs :: String -> [SinkInput]
parseSinkInputs text = snd $ foldr step (Nothing, []) (reverse $ lines text)
  where
    step line r@(Nothing, inputs) = case T.splitOn "Sink Input #" (T.pack line) of
      ["", number] -> (, inputs) . rightToMaybe $ fst <$> T.decimal number
      _ -> r
    step line r@(Just num, inputs) = case T.splitOn "application.name = " (T.stripStart $ T.pack line) of
      [_, appName] -> (Nothing, SinkInput num appName : inputs)
      _ -> r


parseSink :: String -> [Sink]
parseSink text = snd $ foldr step (Nothing, []) (fmap T.pack $ reverse $ lines text)
  where
    step line r@(Nothing, sinks) = case T.splitOn "Name: " (T.stripStart line) of
      ["", sinkName] -> (Just sinkName, sinks)
      _ -> r
    step line r@(Just sinkName, sinks) = case T.splitOn "Description: " (T.stripStart line) of
      ["", sinkDescription] -> (Nothing, Sink sinkName sinkDescription : sinks)
      _ -> r


pulseMove :: IO ()
pulseMove = do
  inputs <- parseSinkInputs <$> listInputs
  sinkInput <- pickOne inputs inputAppName
  case sinkInput of
    Nothing -> pure ()
    Just sinkInput' -> do
      sink <- (parseSink <$> listSinks) >>= flip pickOne sinkDescription
      case sink of
        Nothing    -> pure()
        Just sink' -> callProcess "pactl" ["move-sink-input", show (inputNumber sinkInput'), T.unpack (sinkName sink')]
  where
    listInputs = readProcess "pactl" ["list", "sink-inputs"] ""
    listSinks = readProcess "pactl" ["list", "sinks"] ""



removeIfExists :: FilePath -> IO ()
removeIfExists path = removeFile path `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = pure ()
      | otherwise = throwIO e


wfRecord :: String -> IO ()
wfRecord slurpCmd = do
  removeIfExists filePath
  window <- readProcess slurpCmd [] ""
  callProcess "alacritty" ["-e", "wf-recorder", "-g", window, "-f", filePath]
  callCommand $ "wl-copy < " <> filePath
  where
    filePath = "/tmp/recording.mp4"


main :: IO ()
main = do
    let choices = [ "keyboard"
                  , "bar invisible"
                  , "bar dock"
                  , "xrandr off"
                  , "xrandr on"
                  , "pres on"
                  , "pres off"
                  , "call"
                  , "emoji"
                  , "mpc"
                  , "pulse move"
                  , "record window"
                  , "record region"
                  ]
    out <- selection choices
    case out of
        "keyboard"      -> keyboard
        "bar invisible" -> barMode "invisible"
        "bar dock"      -> barMode "dock"
        "xrandr off"    -> xrandrOff
        "xrandr on"     -> xrandrOn
        "pres on"       -> presOn
        "pres off"      -> presOff
        "call"          -> callContacts
        "emoji"         -> selectEmoji
        "mpc"           -> selectSong
        "pulse move"    -> pulseMove
        "record window" -> wfRecord "slurp-win"
        "record region" -> wfRecord "slurp"
        _               -> putStrLn $ "Invalid selection " ++ out
