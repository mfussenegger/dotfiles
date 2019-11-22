if executable('rls')
  let g:LanguageClient_serverCommands.rust = ['rls']
  source ~/.config/nvim/lsp.vim
endif

if executable('rustfmt')
  setlocal formatprg=rustfmt
endif
