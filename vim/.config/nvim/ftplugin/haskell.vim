setlocal omnifunc=necoghc#omnifunc
" Show type signature in suggestions
let g:necoghc_enable_detailed_browse = 1

nnoremap <silent> <leader>ht :GhcModType<CR>
nnoremap <silent> <leader>hT :GhcModTypeInsert<CR>
nnoremap <silent> <leader>hc :GhcModTypeClear<CR>

let g:ale_linters.haskell = ['ghc-mod', 'hlint']

if executable("stylish-haskell")
    setlocal formatprg=stylish-haskell
endif
