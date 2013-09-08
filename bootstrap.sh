#!/bin/bash

if [[ ! -d $HOME/.config/ ]]; then
    mkdir $HOME/.config
fi

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

if [[ ! -d $HOME/.pip_cache/ ]]; then
    mkdir $HOME/.pip_cache
fi
