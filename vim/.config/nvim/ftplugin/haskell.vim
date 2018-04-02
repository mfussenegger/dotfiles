let g:ale_linters.haskell = ['hlint']

if executable("stylish-haskell")
    setlocal formatprg=stylish-haskell
endif

setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal makeprg=stack\ build
setlocal errorformat=%f:%l:%v:%m

source ~/.config/nvim/lsp.vim
