inoremap <buffer> <c-h> <esc><c-w>h
inoremap <buffer> <c-j> <esc><c-w>j
inoremap <buffer> <c-k> <esc><c-w>k
inoremap <buffer> <c-l> <esc><c-w>l

inoremap <buffer> <F3> <ESC>:lua require'dap'.terminate(); vim.cmd('startinsert!')<CR>
inoremap <buffer> <F5> <ESC>:lua require'dap'.continue(); vim.cmd('startinsert!')<CR>

nnoremap <buffer> gF <c-w>sgF

setlocal nonumber norelativenumber cc=-1 nocuc
setlocal tagfunc=v:lua.require'me.lsp'.symbol_tagfunc

lua require('dap.ext.autocompl').attach()
