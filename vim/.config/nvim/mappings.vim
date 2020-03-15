nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

noremap <leader>o :only<CR>
nnoremap <bs> <c-^>

" pre-filter history navigation in command-mode with already typed input
cnoremap <c-n> <down>
cnoremap <c-p> <up>

nmap ]w <Plug>(ale_next)
nmap [w <Plug>(ale_previous)
nnoremap ]q :cnext<CR>
nnoremap [q :cprevious<CR>
nnoremap ]Q :cfirst<CR>
nnoremap [Q :clast<CR>
nnoremap ]l :lnext<CR>
nnoremap [l :lprevious<CR>
nnoremap ]L :lfirst<CR>
nnoremap [L :llast<CR>


xmap gl <Plug>(EasyAlign)

inoremap <expr> <ESC> (pumvisible() ? "\<c-e>" : "\<ESC>")
inoremap <expr> <CR> (luaeval("require'lsp-ext'.accept_pum()") ? "\<c-y>" : "\<CR>")

" split navigation
noremap <c-h> <c-w>h
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l

" :terminal stuff

tnoremap <c-h> <C-\><C-n><C-w>h
tnoremap <c-j> <C-\><C-n><C-w>j
tnoremap <c-k> <C-\><C-n><C-w>k
tnoremap <c-l> <C-\><C-n><C-w>l

" tn -> terminal new
" te -> terminal execute
" ts -> terminal send
nnoremap <silent><leader>tn :lua require('term').toggle()<CR>
nnoremap <silent><leader>te :w<CR> :lua require('term').run()<CR>
nnoremap <silent><leader>ts :lua require('term').sendLine(vim.fn.getline('.'))<CR>
vnoremap <silent><leader>ts :lua require('term').sendSelection()<CR>

cnoremap <C-a> <Home>
cnoremap <C-e> <End>

augroup outline
  autocmd! FileType python,java nnoremap <buffer><silent>gO :Vista!!<CR>
augroup end


" fzf
nnoremap <silent><leader>f/ :History/<CR>
nnoremap <silent><leader>f: :History:<CR>
nnoremap <silent><leader>ff :Files<CR>
nnoremap <silent><leader>fb :Buffers<CR>
nnoremap <silent><leader>ft :BTags<CR>
nnoremap <silent><leader>fT :Tags<CR>
nnoremap <silent><leader>fg :GitFiles<CR>
nnoremap <silent><leader>gl :BCommits<CR>

" Fuzzy insert mode completion for lines
imap <c-x><c-l> <plug>(fzf-complete-line)

" gnupg
nnoremap <leader>pe :GPGEditRecipients<cr>
nnoremap <leader>pv :GPGViewRecipients<cr>

" fugitive {{{

nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gc :Gcommit -v<cr>

" }}}


" vim-test
nmap <silent> t<C-n> :TestNearest<CR>
nmap <silent> t<C-f> :TestFile<CR>
nmap <silent> t<C-s> :TestSuite --verbose<CR>
nmap <silent> t<C-l> :TestLast<CR>
nmap <silent> t<C-g> :TestVisit<CR>
