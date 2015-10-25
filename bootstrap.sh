#!/bin/bash

test -d $HOME/.config || mkdir $HOME/.config

stow bin
stow python
stow shell
stow vim
stow i3
stow git
stow mplayer
stow termite

$HOME/bin/hopp.py

echo 'run "antigen-hs-compile" in new shell to complete setup'
