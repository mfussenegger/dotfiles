if executable('html-languageserver')
  let g:LanguageClient_serverCommands.html = ['html-languageserver', '--stdio']
  source ~/.config/nvim/lsp.vim
endif
