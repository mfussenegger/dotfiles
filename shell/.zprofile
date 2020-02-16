PATH=$PATH:/home/$USER/bin
PATH=$PATH:/home/$USER/.local/bin/
PATH=$PATH:/home/$USER/.cargo/bin/

export MOZ_USE_XINPUT2=1
export QT_AUTO_SCREEN_SCALE_FACTOR=1

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
