PATH=$PATH:/home/$USER/bin
PATH=$PATH:/home/$USER/.local/bin/
PATH=$PATH:/home/$USER/.gem/ruby/2.2.0/bin/

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
