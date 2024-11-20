nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

noremap <silent><leader>o :lua require('me').only()<CR>
nnoremap <bs> <c-^>

" pre-filter history navigation in command-mode with already typed input
cnoremap <c-n> <down>
cnoremap <c-p> <up>

xmap gl <Plug>(EasyAlign)

nnoremap gf gfzv
nnoremap gF gFzv

nnoremap L Lzz
nnoremap H Hzz

" snippets
imap <C-j> <cmd>lua require('me.snippet').maybe()<CR>


" split navigation
noremap <c-h> <c-w>h
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l
nnoremap <silent> <c-w>s :lua require('me.win').split()<CR>
nnoremap <silent> <c-w>v :lua require('me.win').vsplit()<CR>
nnoremap <silent> <c-w>] :lua require('me.win').goto_def()<CR>


" :terminal stuff

tnoremap <c-h> <C-\><C-n><C-w>h
tnoremap <c-j> <C-\><C-n><C-w>j
tnoremap <c-k> <C-\><C-n><C-w>k
tnoremap <c-l> <C-\><C-n><C-w>l

" tn -> terminal new
" tr -> terminal repl
" te -> terminal execute
" ts -> terminal send
nnoremap <silent><leader>tn :lua require('me.term').toggle()<CR>
nnoremap <silent><leader>tr :lua require('me.term').repl()<CR>
nnoremap <silent><leader>te :w<CR> :lua vim.schedule_wrap(require('me.term').run)()<CR>
nnoremap <silent><leader>ts :lua require('me.term').send_line(vim.fn.getline('.'))<CR>
vnoremap <silent><leader>ts <ESC>:lua require('me.term').send_selection()<CR>

cnoremap <C-a> <Home>
cnoremap <C-e> <End>


" fugitive {{{

nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gd :Gdiffsplit<cr>
nnoremap <leader>gs :Git<cr>
nnoremap <leader>gb :Git blame<cr>
nnoremap <leader>gc :Git commit -v<cr>
nnoremap <leader>gl :G log -n 30 --oneline<cr>
nnoremap <leader>gj :cgete system("git jump --stdout diff")<CR>

" }}}


" for :R digraphs
func! ReadExCommandOutput(newbuf, cmd) abort
  redir => l:message
  silent! execute a:cmd
  redir END
  if a:newbuf | wincmd n | endif
  silent put=l:message
endf
command! -nargs=+ -bang -complete=command R call ReadExCommandOutput(<bang>0, <q-args>)

command -nargs=0 -bar Errors :lua vim.diagnostic.setqflist { title = "Errors", severity = vim.diagnostic.severity.ERROR }
command -nargs=0 -bar Warnings :lua vim.diagnostic.setqflist { title = "Warnings", severity = vim.diagnostic.severity.WARN }

nnoremap <silent> <leader>h :lua require('hop').hint_words()<CR>
nnoremap <silent> <leader>/ :HopPattern<CR>
xnoremap <silent> <leader>h :lua require('hop').hint_words()<CR>
omap     <silent> h :HopWord<CR>
omap     <silent> m :<C-U>lua require('tsht').nodes()<CR>
xnoremap <silent> m :lua require('tsht').nodes()<CR>

nnoremap <space> <cmd>lua vim.diagnostic.open_float({ border = 'single' })<CR>
