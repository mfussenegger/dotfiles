#!/usr/bin/env stack
{- stack script --optimize --resolver lts-15.0  -}

{-# LANGUAGE DeriveGeneric #-}


import           Data.Aeson                 (FromJSON, ToJSON, decode,
                                             eitherDecode)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Char                  (isSpace)
import           Data.Foldable              (for_)
import           Data.List                  (isInfixOf, permutations)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (Maybe, catMaybes, fromJust,
                                             fromMaybe)
import           GHC.Generics               (Generic)
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import           System.Directory           (canonicalizePath, doesFileExist,
                                             getHomeDirectory, removeFile)
import           System.Environment         (getArgs)
import           System.FilePath            ((</>))
import           System.Posix.Files         (createSymbolicLink, rename)
import           System.Process             (callProcess, readProcess)
import           Text.Regex.PCRE            ((=~))


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
isActiveOutput = (=~ ".* connected( primary)? (\\d)+x(\\d)+\\+\\d+\\+\\d+ .*")

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
  changeVimColorScheme ("github", "gruvbox") ("light", "dark")
  setTermiteConfig "config_dark"


presOn = do
  xset ["s", "off"]
  xset ["-dpms"]
  changeVimColorScheme ("gruvbox", "github") ("dark", "light")
  setTermiteConfig "config_light"


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


setTermiteConfig configName = do
  configDir <- expandUser "~/.config/termite/" >>= canonicalizePath
  let 
    targetConfig = configDir </> "config"
  removeFile targetConfig
  createSymbolicLink configName targetConfig
  callProcess "killall" ["-s", "USR1", "termite"]


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
        _                 -> putStrLn $ "Invalid selection " ++ out
