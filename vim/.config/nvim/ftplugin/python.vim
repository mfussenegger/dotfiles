compiler pyunit

" Allows to call :make in any python file that has something like
" if __name__ == "__main__":
"     from unittest import main
"     main()
"
" in it.
" It will then run the tests in the file and populate the quickfix windows
" with which it is possible to navigate to the failing tests
setlocal makeprg=python\ %

if has('nvim-0.5')
  lua require('util').init_hl('python')

  nnoremap <silent> <leader>dn :lua require('dap-python').test_method()<CR>
  vnoremap <silent> <leader>ds <ESC>:lua require('dap-python').debug_selection()<CR>
endif
