alias g='git'
alias gf='git fetch'
alias gl='git log --topo-order --pretty=format:"%C(bold)%C(reset)%C(green)%H%C(red)%d%n%C(bold)%C(reset)%C(cyan)%an <%ae>%n%C(bold)%C(reset)%C(blue)%ai (%ar)%C(reset)%n%+B"'
alias gc='git commit --verbose'
alias gcf='git commit --amend --reuse-message HEAD'
alias gia='git add'
alias gws='git status --ignore-submodules=all --short'
alias gwR='git reset --hard'
alias gir='git reset'


gh-cd() {
  local path=$(gh-maybe-clone.hs "$1")
  cd "$path"
}


gb() {
  git rev-parse HEAD > /dev/null 2>&1 || return
  git branch | grep -v '/HEAD\s' |
    fzf \
      --multi \
      --height 50% \
      --no-sort \
      --preview-window right:60%:noborder \
      --preview 'git log --oneline --pretty="format:%s" $(sed s/^..// <<< {} | cut -d" " -f1) | head -'$LINES | 
    sed 's/^..//' | cut -d' ' -f1
}

gt() {
  git rev-parse HEAD > /dev/null 2>&1 || return
  git tag |
    fzf \
      --multi \
      --height 50% \
      --no-sort \
      --preview-window right:70%:noborder \
      --preview 'git show --color=always {} | head -'$LINES
}

join-lines() {
  local item
  while read item; do
    echo -n "${(q)item} "
  done
}

bind-git-helper() {
  local c
  for c in $@; do
    eval "fzf-g$c-widget() { local result=\$(g$c | join-lines); zle reset-prompt; LBUFFER+=\$result }"
    eval "zle -N fzf-g$c-widget"
    eval "bindkey '^g^$c' fzf-g$c-widget"
  done
}
bind-git-helper b t
unset -f bind-git-helper
