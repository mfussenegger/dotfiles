#!/bin/bash

test -d $HOME/.config || mkdir $HOME/.config

ln -sn ${PWD}/zprezto $HOME/.zprezto

ln -sn ${PWD}/zprezto/runcoms/zlogin $HOME/.zlogin
ln -sn ${PWD}/zprezto/runcoms/zlogout $HOME/.zlogout
ln -sn ${PWD}/zprezto/runcoms/zpreztorc $HOME/.zpreztorc
ln -sn ${PWD}/zprezto/runcoms/zprofile $HOME/.zprofile
ln -sn ${PWD}/zprezto/runcoms/zshenv $HOME/.zshenv
ln -sn ${PWD}/zprezto/runcoms/zshrc $HOME/.zshrc

stow bin
stow python
stow shell
stow vim
stow pentadactyl
stow i3
stow git
stow mplayer
stow dwb

$HOME/bin/peru sync

test -d $HOME/.pip_cache || mkdir $HOME/.pip_cache
