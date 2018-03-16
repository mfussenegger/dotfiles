let g:ale_linters.haskell = ['hlint']

if executable("stylish-haskell")
    setlocal formatprg=stylish-haskell
endif

nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> crr :call LanguageClient_textDocument_rename()<CR>
nnoremap <silent> <a-CR> :call LanguageClient_textDocument_codeAction()<CR>

setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal makeprg=stack\ build
setlocal errorformat=%f:%l:%v:%m
