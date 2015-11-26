{-# LANGUAGE OverloadedStrings #-}
module MyAntigen where

import Antigen (
                -- Rudimentary imports
                AntigenConfig (..)
              , defaultConfig
              , bundle
              , antigen
              )

bundles =
  [ bundle "mafredri/zsh-async"
  , bundle "sindresorhus/pure"
  , bundle "zsh-users/zsh-syntax-highlighting"
  , bundle "zsh-users/zsh-history-substring-search"
  ]

config = defaultConfig { plugins = bundles }

main :: IO ()
main = antigen config
