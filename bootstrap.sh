#!/bin/bash

# folders
ln -s ${PWD}/config/i3status $HOME/.config/i3status
ln -s ${PWD}/config/systemd $HOME/.config/systemd
ln -s ${PWD}/i3 $HOME/.i3
ln -s ${PWD}/mplayer $HOME/.mplayer
ln -s ${PWD}/vim $HOME/.vim
ln -s ${PWD}/zprezto $HOME/.zprezto

# files
ln -s ${PWD}/config/myremote $HOME/.config/myremote
ln -s ${PWD}/Xresources $HOME/.Xresources
ln -s ${PWD}/gitconfig $HOME/.gitconfig
ln -s ${PWD}/gitignore $HOME/.gitignore
ln -s ${PWD}/inputrc $HOME/.inputrc
ln -s ${PWD}/lircrc $HOME/.lircrc
ln -s ${PWD}/pentadactylrc $HOME/.pentadactylrc
ln -s ${PWD}/pythonrc.py $HOME/.pythonrc.py
ln -s ${PWD}/screenrc $HOME/.screenrc
ln -s ${PWD}/vimrc $HOME/.vimrc
ln -s ${PWD}/xinitrc $HOME/.xinitrc
ln -s ${PWD}/zprezto/runcoms/zlogin $HOME/.zlogin
ln -s ${PWD}/zprezto/runcoms/zlogout $HOME/.zlogout
ln -s ${PWD}/zprezto/runcoms/zpreztorc $HOME/.zpreztorc
ln -s ${PWD}/zprezto/runcoms/zprofile $HOME/.zprofile
ln -s ${PWD}/zprezto/runcoms/zshenv $HOME/.zshenv
ln -s ${PWD}/zprezto/runcoms/zshrc $HOME/.zshrc
ln -s ${PWD}/zshrc.local $HOME/.zshrc.local
ln -s ${PWD}/nose.cfg $HOME/nose.cfg

echo "PATH=\$PATH:${PWD}/bin" > $HOME/.zshrc.local.dyn
