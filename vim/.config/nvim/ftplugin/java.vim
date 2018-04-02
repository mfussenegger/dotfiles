if filereadable("./gradlew")
    setlocal makeprg=./gradlew\ --no-daemon\ -q
else
    setlocal makeprg=gradle\ --no-daemon\ -q
endif

source ~/.config/nvim/lsp.vim
