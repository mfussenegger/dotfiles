#!/bin/bash

test -d $HOME/.config || mkdir $HOME/.config

stow bin
stow python
stow shell
stow vim
stow intellij
stow i3
stow git
stow mplayer
stow termite

$HOME/bin/hopp.py -c hopp/vim.json hopp/misc.json

if [[ -x '/usr/bin/cabal' ]]; then
    cabal update
    cabal install install base text directory filepath process
    echo 'run "antigen-hs-compile" in new shell to complete setup'
else
    echo "cabal (and maybe haskell (ghc)?) not installed. Required for antigen-hs"
fi
