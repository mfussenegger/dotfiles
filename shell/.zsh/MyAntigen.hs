{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module MyAntigen where

import Antigen (
                -- Rudimentary imports
                AntigenConfiguration (..)
              , bundle
              , antigen
              )
import Shelly (shelly)

bundles =
  [ bundle "mafredri/zsh-async"
  , bundle "sindresorhus/pure"
  , bundle "zsh-users/zsh-syntax-highlighting"
  , bundle "zsh-users/zsh-history-substring-search"
  ]

config = AntigenConfiguration bundles

main :: IO ()
main = shelly $ antigen config
