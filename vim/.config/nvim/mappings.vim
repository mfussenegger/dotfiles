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

" Close popup menu *and* start a new line when pressing <Enter>
inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")

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
nnoremap <leader>tn :below new term://
nnoremap <silent><leader>te :w<CR> :below new term://%:p<CR>
nnoremap <silent><leader>ts yy<c-w>wp<c-w>pgv
vnoremap <silent><leader>ts y<c-w>wp<c-w>pgv

cnoremap <C-a> <Home>
cnoremap <C-e> <End>

augroup outline
  autocmd! FileType python,java nnoremap <buffer><silent>gO :TagbarToggle<CR>
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
nnoremap <leader>gc :Gcommit<cr>

" }}}


nmap <silent> t<C-n> :TestNearest<CR>
nmap <silent> t<C-f> :TestFile<CR>
nmap <silent> t<C-s> :TestSuite --verbose<CR>
nmap <silent> t<C-l> :TestLast<CR>
nmap <silent> t<C-g> :TestVisit<CR>
