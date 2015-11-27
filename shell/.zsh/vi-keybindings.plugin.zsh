# Vi
bindkey -v

autoload edit-command-line; zle -N edit-command-line
bindkey -M vicmd v edit-command-line

bindkey -M vicmd "k" history-substring-search-up
bindkey -M vicmd "j" history-substring-search-down
