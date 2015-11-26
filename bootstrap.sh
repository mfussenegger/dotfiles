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
