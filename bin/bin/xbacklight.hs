#!/usr/bin/stack
-- stack script --optimize --resolver lts-17.4
--
-- To allow users to run this, create a udev rule:
--
-- /etc/udev/rules.d/backlight.rules
-- ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="/bin/chgrp video /sys/class/backlight/%k/brightness"
-- ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="/bin/chmod g+w /sys/class/backlight/%k/brightness"
--
-- And add the user to the video group

import           Control.Monad       (when)
import           Data.Char           (isDigit)
import           Options.Applicative
import           Text.Printf         (printf)
import           Text.Read           (readEither)


brightnessFile :: String
brightnessFile = "/sys/class/backlight/intel_backlight/brightness"


maxBrightness :: IO Int
maxBrightness =
  read <$> readFile "/sys/class/backlight/intel_backlight/max_brightness"


currentBrightness :: IO Int
currentBrightness =
  read <$> readFile brightnessFile


changeBrightness :: Action -> IO ()
changeBrightness action = do
  curr <- currentBrightness
  limit <- maxBrightness
  let
    diff = case action of
      (Inc step) -> diffVal step
      (Dec step) -> diffVal step * (-1)
    diffVal (Percentage pct) = round $ fromIntegral limit / 100.0 * fromIntegral pct
    diffVal (Value val)      = val
    newVal = max 0 (min (curr + diff) limit)
  -- force lazy io to avoid openFile: resource busy error
  when (curr >= 0) $
    writeFile brightnessFile (show newVal)


data Step = Percentage Int
          | Value Int
          deriving (Show)


parseStep :: ReadM Step
parseStep = eitherReader toStep
  where
    toStep :: String -> Either String Step
    toStep xs
      | last xs == '%' = Right $ Percentage (read (takeWhile isDigit xs))
      | otherwise      = Value <$> readEither xs


data Action
  = Inc Step
  | Dec Step
  deriving (Show)


incAction :: Parser Action
incAction = Inc <$> option parseStep ( long "inc" )


decAction :: Parser Action
decAction = Dec <$> option parseStep ( long "dec" )


actionParser :: Parser Action
actionParser = incAction <|> decAction


main :: IO ()
main = do
  args <- execParser opts
  changeBrightness $ args
  where
    opts = info (actionParser <**> helper)
      ( fullDesc <> progDesc "Increase or decrease backlight" )
