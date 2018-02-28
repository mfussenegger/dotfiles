let g:ale_linters.haskell = ['hlint']

if executable("stylish-haskell")
    setlocal formatprg=stylish-haskell
endif

nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
