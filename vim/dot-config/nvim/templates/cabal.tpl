cabal-version:      2.4
name:               TODO
version:            0.1.0.0
license:            GPL-3.0-only
author:             Mathias Fussenegger

extra-source-files: CHANGES.md


flag static
  description: Pass -static to ghc when linking the binary.
  manual: True
  default: False

common deps
  ghc-options: -Wall
  build-depends:
      base
    , text
    , bytestring

library
  import: deps
  default-language: Haskell2010
  hs-source-dirs:   src
  exposed-modules:
    TODO.Main
  other-modules:

executable TODO
  import: deps
  main-is:          Main.hs
  build-depends:    TODO
  hs-source-dirs:   app
  default-language: Haskell2010
  if flag(static)
    ghc-options: fPIC
    ld-options: -static
