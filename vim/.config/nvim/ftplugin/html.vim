" Markdown runtime files load html ftplugin files -> bail out to prevent lsp setup
if &filetype != 'html'
  finish
endif

if executable('html-languageserver')
  let g:LanguageClient_serverCommands.html = ['html-languageserver', '--stdio']
  source ~/.config/nvim/lsp.vim
endif
