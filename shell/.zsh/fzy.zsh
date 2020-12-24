source ~/.zsh/plugins/zsh-fzy.plugin.zsh

zstyle :fzy:file    command      fd

bindkey ''  fzy-file-widget
bindkey ''  fzy-history-widget
