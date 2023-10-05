PURE_PROMPT_SYMBOL="↪ "
PURE_GIT_PULL=0
PURE_GIT_UNTRACKED_DIRTY=0
FZF_DEFAULT_COMMAND="rg --files"

source ~/.zsh/history.plugin.zsh
source ~/.zsh/completion.plugin.zsh
source ~/.zsh/systemd.plugin.zsh
source ~/.zsh/utils.plugin.zsh
source ~/.zsh/python.plugin.zsh
source ~/.zsh/aliases.plugin.zsh
source ~/.zsh/vi-keybindings.plugin.zsh
source ~/.zsh/git.plugin.zsh
source ~/.zsh/fzy.zsh
source ~/.zshrc.local

source ~/.zsh/plugins/async.zsh
source ~/.zsh/plugins/zsh-history-substring-search.zsh
source ~/.zsh/plugins/pure.zsh

[ -f ~/.zshrc.secret ] && source ~/.zshrc.secret

[ -f ~/.dircolors ] && eval $(dircolors ~/.dircolors)
eval "$(lua ~/.zsh/plugins/z.lua --init zsh once enhanced)"
