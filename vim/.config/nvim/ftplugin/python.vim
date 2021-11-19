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
  nnoremap <silent> <leader>dn :lua require('me.dap'); require('dap-python').test_method()<CR>
  nnoremap <silent> <leader>df :lua require('me.dap'); require('dap-python').test_class()<CR>
  vnoremap <silent> <leader>ds <ESC>:lua require('me.dap'); require('dap-python').debug_selection()<CR>
endif
