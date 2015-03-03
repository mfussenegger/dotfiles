PATH=$PATH:/home/$USER/bin/
PATH=$PATH:/home/$USER/.local/bin/
PATH=$PATH:/home/$USER/.gem/ruby/2.1.0/bin/

PURE_GIT_PULL=0
PURE_GIT_UNTRACKED_DIRTY=0

source ~/.zsh/antigen-hs/init.zsh


source ~/.zsh/history.plugin.zsh
source ~/.zsh/completion.plugin.zsh
source ~/.zsh/systemd.plugin.zsh
source ~/.zsh/git.plugin.zsh


eval "$(fasd --init auto)"
alias j='fasd_cd -i'


source ~/.zshrc.local

if (( $+commands[envoy] )); then
    source <(envoy -p)
fi


# Vi
bindkey -v

bindkey -M vicmd "k" history-substring-search-up
bindkey -M vicmd "j" history-substring-search-down

alias ls='ls --group-directories-first --color=auto'
alias ll='ls -lh'
alias la='ll -A'
alias lt='ll -tr'
alias lu='lt -u'

function mcd() {
    [[ -n "$1" ]] && mkdir -p "$1" && builtin cd "$1"
}
