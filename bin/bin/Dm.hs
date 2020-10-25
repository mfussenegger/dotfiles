#!/usr/bin/env stack
{- stack script --optimize --resolver lts-16.12  -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


import Data.Aeson
  ( FromJSON,
    ToJSON,
    decode,
    eitherDecode,
  )
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.List (isInfixOf)
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
    getHomeDirectory,
  )
import System.Environment (getArgs)
import System.Posix.Files (rename)
import System.Process (callProcess, readProcess)
import Text.Regex.PCRE ((=~))
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Either.Combinators (rightToMaybe)
import Text.Printf (printf)


selection :: [String] -> IO String
selection input = do
  args <- getArgs
  let
    runTool tool args = trim <$> readProcess tool args (unlines input)
    tool = case args of
      [] -> "dmenu"
      [arg] -> arg
  if tool == "dmenu"
    then runTool "dmenu" dmenuArgs
    else runTool tool []
  where
    dmenuArgs = [ "-l", "30"
                , "-fn", "-*-terminus-medium-*-*-*-14-140-*-*-*-*-*-*" ]


pickOne :: [a] -> (a -> String) -> IO (Maybe a)
pickOne xs formatX = do
  selected <- selection formattedXs
  case T.splitOn " ¦ " (T.pack selected) of
    (idx : _) -> pure . fmap (xs !!) . rightToMaybe $ fst <$> T.decimal idx
    _         -> pure Nothing
  where
    formattedXs = fmap renderX (zip [(0 :: Int)..] xs)
    renderX (idx, x) = printf "%03d ¦ %s" idx (formatX x)



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


i3barMode :: String -> IO ()
i3barMode mode = callProcess "i3-msg" (["bar", "mode"] ++ [mode])


isActiveOutput :: String -> Bool
isActiveOutput = (=~ (".* connected( primary)? (\\d)+x(\\d)+\\+\\d+\\+\\d+ .*" :: String))

isAvailableOutput line = not (isActiveOutput line) && " connected" `isInfixOf` line


getOutputs :: (String -> Bool) -> String -> [String]
getOutputs predicate xrandrOutput =
  let
    filteredOutput = filter predicate $ lines xrandrOutput
  in
    map (takeWhile (not . isSpace)) filteredOutput


xrandrOn = do
  xrandrOutput <- readProcess "xrandr" [] ""
  case getOutputs isAvailableOutput xrandrOutput of
    []       -> putStrLn "No inactive output"
    [output] -> activeOutput xrandrOutput output
    outputs  -> selection outputs >>= activeOutput xrandrOutput


activeOutput xrandrOutput newOutput = do
  let 
    activeOutputs = getOutputs isActiveOutput xrandrOutput
    makeChoices output = map (\x -> x ++ " " ++ output) ["--left-of", "--right-of", "--below", "--above", "--same-as"]
    choices = concatMap makeChoices activeOutputs
  choice <- break isSpace <$> selection choices
  callProcess "xrandr" ["--output", newOutput, fst choice, trim $ snd choice, "--auto"]


xrandrOff = do
  output <- getOutputs isActiveOutput <$> readProcess "xrandr" [] ""
            >>= selection
  callProcess "xrandr" ["--output", output, "--off"]


xset = callProcess "xset"


presOff = do
  xset ["s", "on"]
  xset ["+dpms"]
  changeVimColorScheme ("tempus_totus", "gruvbox8_hard") ("light", "dark")
  alacrittyConfig <- expandUser "~/.config/alacritty/alacritty.yml" >>= canonicalizePath
  sed alacrittyConfig ("colors: *light") ("colors: *dark")
  sed alacrittyConfig ("    family: JetBrains Mono") ("    family: JetBrains Mono Light")


presOn = do
  xset ["s", "off"]
  xset ["-dpms"]
  changeVimColorScheme ("gruvbox8_hard", "tempus_totus") ("dark", "light")
  alacrittyConfig <- expandUser "~/.config/alacritty/alacritty.yml" >>= canonicalizePath
  sed alacrittyConfig ("colors: *dark") ("colors: *light")
  sed alacrittyConfig ("    family: JetBrains Mono Light") ("    family: JetBrains Mono")


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


changeVimColorScheme colorscheme background = do
  vimrc <- expandUser "~/.config/nvim/options.vim" >>= canonicalizePath
  sed vimrc ("colorscheme " ++ fst colorscheme) ("colorscheme " ++ snd colorscheme)
  sed vimrc ("set background=" ++ fst background) ("set background=" ++ snd background)
  instances <- lines <$> readProcess "nvr" ["--serverlist"] ""
  let changeColor = "<Esc>:set background=" ++ snd background ++ "<CR>:colorscheme " ++ snd colorscheme ++ "<CR>"
  for_ instances (\x ->
    callProcess "nvr" ["--servername", x, "--remote-send", changeColor])


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


instance ToJSON Emoji
instance FromJSON Emoji


selectEmoji :: IO ()
selectEmoji = do
  emojiFileContents <- fromUriOrCache cachePath source
  let
    emojis = catMaybes <$> (eitherDecode emojiFileContents :: Either String [Maybe Emoji])
  case emojis of
    (Left err)-> error $ "Couldn't decode " <> cachePath <> ": " <> err
    (Right emojis') -> do
      selected <- selection (fmap description emojis')
      let
        selectedEmoji = emoji . head $ filter (\x -> description x == selected) emojis'
      putStrLn selectedEmoji
      callProcess "xdotool" ["type", "--delay", "100", selectedEmoji]

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
      [_, appName] -> (Nothing, SinkInput num (appName) : inputs)
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
  sinkInput <- pickOne inputs (T.unpack . inputAppName)
  case sinkInput of
    Nothing -> pure ()
    Just sinkInput' -> do
      sink <- (parseSink <$> listSinks) >>= flip pickOne (T.unpack . sinkDescription)
      case sink of
        Nothing    -> pure()
        Just sink' -> callProcess "pactl" ["move-sink-input", show (inputNumber sinkInput'), T.unpack (sinkName sink')]
  where
    listInputs = readProcess "pactl" ["list", "sink-inputs"] ""
    listSinks = readProcess "pactl" ["list", "sinks"] ""


main :: IO ()
main = do
    let choices = [ "keyboard"
                  , "i3bar invisible"
                  , "i3bar dock"
                  , "xrandr off"
                  , "xrandr on"
                  , "pres on"
                  , "pres off"
                  , "call" 
                  , "emoji"
                  , "mpc"
                  , "pulse move"
                  ]
    out <- selection choices
    case out of
        "keyboard"        -> keyboard
        "i3bar invisible" -> i3barMode "invisible"
        "i3bar dock"      -> i3barMode "dock"
        "xrandr off"      -> xrandrOff
        "xrandr on"       -> xrandrOn
        "pres on"         -> presOn
        "pres off"        -> presOff
        "call"            -> callContacts
        "emoji"           -> selectEmoji
        "mpc"             -> selectSong
        "pulse move"      -> pulseMove
        _                 -> putStrLn $ "Invalid selection " ++ out
