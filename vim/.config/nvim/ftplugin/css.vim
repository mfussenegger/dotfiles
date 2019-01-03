if executable('css-languageserver')
  let g:LanguageClient_serverCommands.css = ['css-languageserver', '--stdio']
  source ~/.config/nvim/lsp.vim
endif
