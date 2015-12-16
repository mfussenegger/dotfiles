PATH=$PATH:/home/$USER/bin/
PATH=$PATH:/home/$USER/.local/bin/
PATH=$PATH:/home/$USER/.gem/ruby/2.2.0/bin/

PURE_PROMPT_SYMBOL="🐘  "
PURE_GIT_PULL=0
PURE_GIT_UNTRACKED_DIRTY=0

source ~/.zsh/antigen-hs/init.zsh

source ~/.zsh/history.plugin.zsh
source ~/.zsh/completion.plugin.zsh
source ~/.zsh/systemd.plugin.zsh
source ~/.zsh/git.plugin.zsh
source ~/.zsh/utils.plugin.zsh
source ~/.zsh/aliases.plugin.zsh
source ~/.zsh/vi-keybindings.plugin.zsh
source ~/.zshrc.local

if (( $+commands[envoy] )); then
    source <(envoy -p)
fi

[[ -f ~/.dircolors ]] && eval $(dircolors ~/.dircolors)

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
