#!/bin/zsh

test -d $HOME/.config || mkdir $HOME/.config
test -d $HOME/.config/nvim || mkdir $HOME/.config/nvim
test -d $HOME/bin || mkdir $HOME/bin

stow bin
stow shell
stow vim
stow intellij
stow i3
stow git
stow termite
stow haskell

awk '/TERM xterm$/ { print; print "TERM xterm-termite"; next }1' <(dircolors -p)  >! ~/.dircolors
$HOME/bin/hopp.py hopp/vim.json hopp/misc.json
