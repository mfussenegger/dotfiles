#!/bin/zsh

test -d $HOME/.config || mkdir $HOME/.config
test -d $HOME/.config/nvim || mkdir $HOME/.config/nvim
test -d $HOME/bin || mkdir $HOME/bin
test -d $HOME/.gnupg || (mkdir $HOME/.gnupg && chmod 700 $HOME/.gnupg)
test -d $HOME/.zsh/completion || mkdir -p $HOME/.zsh/completion

stow bin
stow shell
stow vim
stow intellij
stow i3
stow git
stow termite
stow haskell
stow psql
stow gnupg

awk '/TERM xterm$/ { print; print "TERM xterm-termite"; next }1' <(dircolors -p)  >! ~/.dircolors
