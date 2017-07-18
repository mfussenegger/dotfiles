PURE_PROMPT_SYMBOL="↪ "
PURE_GIT_PULL=0
PURE_GIT_UNTRACKED_DIRTY=0

source ~/.zsh/history.plugin.zsh
source ~/.zsh/completion.plugin.zsh
source ~/.zsh/systemd.plugin.zsh
source ~/.zsh/git.plugin.zsh
source ~/.zsh/utils.plugin.zsh
source ~/.zsh/python.plugin.zsh
source ~/.zsh/aliases.plugin.zsh
source ~/.zsh/vi-keybindings.plugin.zsh
source ~/.zshrc.local

[[ -f ~/.dircolors ]] && eval $(dircolors ~/.dircolors)
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source ~/.zsh/plugins/z.sh
source ~/.zsh/plugins/async.zsh
source ~/.zsh/plugins/zsh-history-substring-search.zsh
source ~/.zsh/plugins/pure.zsh
