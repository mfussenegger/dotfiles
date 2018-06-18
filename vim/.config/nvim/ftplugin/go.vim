if filereadable($GOPATH . '/bin/go-langserver')
  let g:LanguageClient_serverCommands.go = [$GOPATH . '/bin/go-langserver', '-gocodecompletion']
  source ~/.config/nvim/lsp.vim
endif
