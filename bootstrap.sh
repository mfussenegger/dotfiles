#!/bin/bash

# folders
ln -sn ${PWD}/config/i3status $HOME/.config/i3status
ln -sn ${PWD}/config/systemd $HOME/.config/systemd
ln -sn ${PWD}/i3 $HOME/.i3
ln -sn ${PWD}/mplayer $HOME/.mplayer
ln -sn ${PWD}/vim $HOME/.vim
ln -sn ${PWD}/zprezto $HOME/.zprezto

# files
ln -sn ${PWD}/config/myremote $HOME/.config/myremote
ln -sn ${PWD}/Xresources $HOME/.Xresources
ln -sn ${PWD}/gitconfig $HOME/.gitconfig
ln -sn ${PWD}/gitignore $HOME/.gitignore
ln -sn ${PWD}/inputrc $HOME/.inputrc
ln -sn ${PWD}/lircrc $HOME/.lircrc
ln -sn ${PWD}/pentadactylrc $HOME/.pentadactylrc
ln -sn ${PWD}/pythonrc.py $HOME/.pythonrc.py
ln -sn ${PWD}/screenrc $HOME/.screenrc
ln -sn ${PWD}/vimrc $HOME/.vimrc
ln -sn ${PWD}/xinitrc $HOME/.xinitrc
ln -sn ${PWD}/zprezto/runcoms/zlogin $HOME/.zlogin
ln -sn ${PWD}/zprezto/runcoms/zlogout $HOME/.zlogout
ln -sn ${PWD}/zprezto/runcoms/zpreztorc $HOME/.zpreztorc
ln -sn ${PWD}/zprezto/runcoms/zprofile $HOME/.zprofile
ln -sn ${PWD}/zprezto/runcoms/zshenv $HOME/.zshenv
ln -sn ${PWD}/zprezto/runcoms/zshrc $HOME/.zshrc
ln -sn ${PWD}/zshrc.local $HOME/.zshrc.local
ln -sn ${PWD}/nose.cfg $HOME/nose.cfg

echo "PATH=\$PATH:${PWD}/bin" > $HOME/.zshrc.local.dyn

if which 'urlview' &> /dev/null ; then
    if [[ "$OSTYPE" == drawin* ]]; then
        echo "COMMAND open %s" >> $HOME/.urlview
    else
        echo "COMMAND xdg-open %s" >> $HOME/.urlview
    fi
fi

if [[ ! -d $HOME/.pip_cache/ ]]; then
    mkdir $HOME/.pip_cache
fi
