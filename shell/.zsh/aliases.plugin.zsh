alias ls='ls --group-directories-first --color=auto'
alias ll='ls -lh'
alias la='ll -A'
alias lt='ll -tr'
alias lu='lt -u'

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

## pacman

alias p='sudo pacman'
alias pac='pkgbuilder'

## yay python smtpd
alias smtpd="python -m smtpd -n -c DebuggingServer localhost:1025"


## global aliases
alias -g L='| less'
alias -g N='2> /dev/null'
alias -g G='| grep'
alias -g H='| head'
alias -g T='| tail'
alias -g F='| fzf'
alias -g X='| xargs'
alias -g V='| vim -'


## misc
alias s='TERM=xterm ssh'
alias c="date && echo -e && cal -3m"
alias :q="exit"
alias eject='sudo eject'
alias rcp='rsync --progress'
alias drop_caches='echo 3 | sudo tee /proc/sys/vm/drop_caches'

# useful defaults
alias rsync="ionice -c 3 rsync"
alias iostat="iostat -mtx"
alias rdesktop="rdesktop -k de -K -z"

alias benice="renice -n 19 -p $$ && ionice -c3 -p $$ && ionice -p $$"
alias berude="sudo renice -n -19 -p $$ && sudo ionice -c1 -p $$ && ionice -p $$"
