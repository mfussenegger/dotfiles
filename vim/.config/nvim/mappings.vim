nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

noremap <silent><leader>o :lua require('me').only()<CR>
nnoremap <bs> <c-^>

" pre-filter history navigation in command-mode with already typed input
cnoremap <c-n> <down>
cnoremap <c-p> <up>

nnoremap ]q :cnext<CR>
nnoremap [q :cprevious<CR>
nnoremap ]Q :cfirst<CR>
nnoremap [Q :clast<CR>
nnoremap ]l :lnext<CR>
nnoremap [l :lprevious<CR>
nnoremap ]L :lfirst<CR>
nnoremap [L :llast<CR>


xmap gl <Plug>(EasyAlign)

if has('nvim-0.5')
  inoremap <expr> <CR> (luaeval("require'lsp_compl'.accept_pum()") ? "\<c-y>" : "\<CR>")
end

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


if has('nvim-0.5')
  nnoremap <silent><leader>fq :lua require('me.fzy').actions.quickfix()<CR>
  nnoremap <silent><leader>ff :lua local fzy = require('me.fzy'); fzy.execute('fd', fzy.sinks.edit_file)<CR>
  nnoremap <silent><leader>fb :lua require('me.fzy').actions.buffers()<CR>
  nnoremap <silent><leader>ft :lua local fzy = require('me.fzy'); fzy.try(fzy.actions.lsp_tags, fzy.actions.buf_tags)<CR>
  nnoremap <silent><leader>fg :lua local fzy = require('me.fzy'); fzy.execute('git ls-files', fzy.sinks.edit_file)<CR>
  nnoremap <silent><leader>f/ :lua require('me.fzy').actions.buf_lines()<CR>
  inoremap <silent><c-e> <ESC>:lua require('me.fzy').emoji()<CR>
endif

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


" vim-test 
nmap <silent> t<C-n> :w <BAR> TestNearest<CR>
nmap <silent> t<C-f> :w <BAR> TestFile<CR>
nmap <silent> t<C-s> :w <BAR> TestSuite --verbose<CR>
nmap <silent> t<C-l> :w <BAR> TestLast<CR>
nmap <silent> t<C-g> :w <BAR> TestVisit<CR>


" for :R digraphs
func! ReadExCommandOutput(newbuf, cmd) abort
  redir => l:message
  silent! execute a:cmd
  redir END
  if a:newbuf | wincmd n | endif
  silent put=l:message
endf
command! -nargs=+ -bang -complete=command R call ReadExCommandOutput(<bang>0, <q-args>)


nnoremap <silent> <F3> :lua require'me.dap'.terminate()<CR>
nnoremap <silent> <F5> :lua require'me.dap'.continue()<CR>
nnoremap <silent> <F10> :lua require'me.dap'.step_over()<CR>
nnoremap <silent> <F11> :lua require'me.dap'.step_into()<CR>
nnoremap <silent> <F12> :lua require'me.dap'.step_out()<CR>
nnoremap <silent> <leader>b :lua require'me.dap'.toggle_breakpoint()<CR>
nnoremap <silent> <leader>B :lua require'me.dap'.toggle_breakpoint(vim.fn.input('Breakpoint Condition: '), nil, nil, true)<CR>
nnoremap <silent> <leader>lp :lua require'me.dap'.toggle_breakpoint(nil, nil, vim.fn.input('Log point message: '), true)<CR>
nnoremap <silent> <leader>dr :lua require'me.dap'.repl.toggle({height=15})<CR>
nnoremap <silent> <leader>dl :lua require('me.dap').run_last()<CR>
nnoremap <silent> <leader>dS :lua local w = require('me.dap').widgets; w.centered_float(w.frames)<CR>
nnoremap <silent> <leader>dt :lua local w = require('me.dap').widgets; w.centered_float(w.threads)<CR>
nnoremap <silent> <leader>ds :lua local w = require('me.dap').widgets; w.centered_float(w.scopes)<CR>
nnoremap <silent> <leader>dh :lua require('me.dap').widgets.hover()<CR>
nnoremap <silent> <leader>dc :lua require('me.dap').run_to_cursor()<CR>
vnoremap <silent> <leader>dh :lua require('me.dap').widgets.hover(require("dap.utils").get_visual_selection_text)<CR>
nnoremap <silent> <leader>dj :lua require'me.dap'.down()<CR>
nnoremap <silent> <leader>dk :lua require'me.dap'.up()<CR>

command -nargs=0 Into :lua require('me.dap').step_into()
command -nargs=0 DapBreakpoints :copen | lua require('me.dap').list_breakpoints()
command -nargs=0 DapSidebar :lua require('me.dap').sidebar.toggle()
command -nargs=0 DapReload :lua require('me').reload_dap()

command -nargs=0 LspErrors :lua vim.diagnostic.setqflist { severity = vim.diagnostic.severity.ERROR }
command -nargs=0 LspWarnings :lua vim.diagnostic.setqflist { severity = vim.diagnostic.severity.WARN }
command -nargs=0 LspRestart :lua require('me.lsp.conf').restart()

nnoremap <silent> <leader>q :lua require('quickfix').toggle()<CR>

nnoremap <silent> <leader>h :lua require('hop').hint_words()<CR>
nnoremap <silent> <leader>/ :HopPattern<CR>
vnoremap <silent> <leader>h :lua require('hop').hint_words()<CR>
omap     <silent> h :HopWord<CR>
omap     <silent> m :<C-U>lua require('tsht').nodes()<CR>
vnoremap <silent> m :lua require('tsht').nodes()<CR>

nnoremap <space> <cmd>lua vim.diagnostic.open_float(0, { border = 'single' })<CR>
nnoremap ]w <cmd>lua vim.diagnostic.goto_next{ float = { border = 'single' }}<CR>
nnoremap [w <cmd>lua vim.diagnostic.goto_prev{ float = { border = 'single' }}<CR>
