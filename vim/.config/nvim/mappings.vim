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
imap <C-j> <cmd>lua U.reload('me.snippet').maybe()<CR>

imap <expr> <Tab>   luasnip#jumpable(1)  ? '<Plug>luasnip-expand-or-jump' : '<Tab>'
smap <expr> <Tab>   luasnip#jumpable(1)  ? '<Plug>luasnip-jump-next' : '<Tab>'
imap <expr> <S-Tab> luasnip#jumpable(-1) ? '<Plug>luasnip-jump-prev' : '<S-Tab>'
smap <expr> <S-Tab> luasnip#jumpable(-1) ? '<Plug>luasnip-jump-prev' : '<S-Tab>'


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
" te -> terminal execute
" ts -> terminal send
nnoremap <silent><leader>tn :lua require('term').toggle()<CR>
nnoremap <silent><leader>te :w<CR> :lua require('term').run()<CR>
nnoremap <silent><leader>ts :lua require('term').sendLine(vim.fn.getline('.'))<CR>
vnoremap <silent><leader>ts <ESC>:lua require('term').sendSelection()<CR>

cnoremap <C-a> <Home>
cnoremap <C-e> <End>

augroup outline
  autocmd! FileType python,java,lua,haskell,yaml nnoremap <buffer><silent>gO :Vista!!<CR>
augroup end


" gnupg
nnoremap <leader>pe :GPGEditRecipients<cr>
nnoremap <leader>pv :GPGViewRecipients<cr>

" fugitive {{{

nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gd :Gdiffsplit<cr>
nnoremap <leader>gs :Git<cr>
nnoremap <leader>gb :Git blame<cr>
nnoremap <leader>gc :Git commit -v<cr>

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

command -nargs=0 LspErrors :lua vim.diagnostic.setqflist { severity = vim.diagnostic.severity.ERROR }
command -nargs=0 LspWarnings :lua vim.diagnostic.setqflist { severity = vim.diagnostic.severity.WARN }
command -nargs=0 LspRestart :lua require('me.lsp.conf').restart()

nnoremap <silent> <leader>h :lua require('hop').hint_words()<CR>
nnoremap <silent> <leader>/ :HopPattern<CR>
vnoremap <silent> <leader>h :lua require('hop').hint_words()<CR>
omap     <silent> h :HopWord<CR>
omap     <silent> m :<C-U>lua require('tsht').nodes()<CR>
vnoremap <silent> m :lua require('tsht').nodes()<CR>

nnoremap <space> <cmd>lua vim.diagnostic.open_float(0, { border = 'single' })<CR>
