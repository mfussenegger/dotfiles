PATH=$PATH:/home/$USER/bin
PATH=$PATH:/home/$USER/.local/bin/
PATH=$PATH:/home/$USER/.cargo/bin/

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
