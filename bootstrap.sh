#!/bin/bash

test -d $HOME/.config || mkdir $HOME/.config

stow bin
stow python
stow shell
stow vim
stow i3
stow git
stow mplayer


$HOME/bin/peru sync

test -d $HOME/.pip_cache || mkdir $HOME/.pip_cache

echo 'run "antigen-hs-compile" in new shell to complete setup'
