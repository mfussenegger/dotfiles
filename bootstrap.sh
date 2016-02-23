#!/bin/zsh

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

awk '/TERM xterm$/ { print; print "TERM xterm-termite"; next }1' <(dircolors -p)  >! ~/.dircolors
$HOME/bin/hopp.py hopp/vim.json hopp/misc.json
