if executable('docker-langserver')
  let g:LanguageClient_serverCommands.dockerfile = ['docker-langserver', '--stdio']
  source ~/.config/nvim/lsp.vim
endif
