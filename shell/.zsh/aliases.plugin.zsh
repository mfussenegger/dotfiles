alias ls='ls --group-directories-first --color=auto'
alias ll='ls -lh'
alias la='ll -A'
alias lt='ll -tr'
alias lu='lt -u'

alias p='sudo pacman'
alias sc='systemctl'

# whitespace at the end causes the next command word after sudo to
# also be checked for alias expansion
alias sudo='sudo '

## lvm
alias lvs='sudo lvs'
alias vgs='sudo vgs'
alias pvs='sudo pvs'
alias lvdisplay='sudo lvdisplay'
alias pvdisplay='sudo pvdisplay'
alias vgdisplay='sudo vgdisplay'

# z.lua
alias zh='z -I -t .'
alias zb='z -b'

## global aliases
alias -g N='2> /dev/null'
alias -g G='| rg'
alias -g J='| jless'
alias -g V='| v -'
if [ -n "$SSH_TTY" ] && hash tty-copy 2> /dev/null; then
  alias -g C='| tty-copy -n'
else
  alias -g C='| wl-copy -n'
fi


## misc
alias s='ssh'
alias c="date -R && echo -e && khal calendar --notstarted"
alias :q="exit"
alias eject='sudo eject'
alias rcp='rsync --progress'
alias lsblk='lsblk -o +MODEL -o +LABEL'
alias drop_caches='echo 3 | sudo tee /proc/sys/vm/drop_caches'

# useful defaults
alias rsync="ionice -c 3 rsync"
alias iostat="iostat -mtx"
alias rdesktop="rdesktop -k de -K -z"
alias ip="ip --color"

alias tablet="scrcpy --v4l2-sink=/dev/video7 --no-display"
