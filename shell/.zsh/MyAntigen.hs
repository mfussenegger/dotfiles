{-# LANGUAGE OverloadedStrings #-}
module MyAntigen where

import Antigen (
                -- Rudimentary imports
                AntigenConfig (..)
              , defaultConfig
              , bundle
              , antigen
              , ZshPlugin (..)
              , antigenSourcingStrategy
              , filePathsSourcingStrategy
              )

bundles =
  [ bundle "mafredri/zsh-async"
  , bundle "sindresorhus/pure"
  , bundle "zsh-users/zsh-syntax-highlighting"
  , bundle "zsh-users/zsh-history-substring-search"
  , bundle "zsh-users/zsh-completions"
  , (bundle "b4b4r07/enhancd") { sourcingStrategy = antigenSourcingStrategy }
  ]

config = defaultConfig { plugins = bundles }

main :: IO ()
main = antigen config
