setlocal formatprg=python\ -m\ json.tool
if executable('json-languageserver')
  let g:LanguageClient_serverCommands.json = ['json-languageserver', '--stdio']
  source ~/.config/nvim/lsp.vim
endif
