#!/bin/bash

rm -f $HOME/.zprezto
rm -f $HOME/.zlogin
rm -f $HOME/.zlogout
rm -f $HOME/.zpreztorc
rm -f $HOME/.zprofile
rm -f $HOME/.zshenv
rm -f $HOME/.zshrc

stow --delete bin
stow --delete python
stow --delete shell
stow --delete vim
stow --delete pentadactyl
stow --delete i3
stow --delete mplayer
