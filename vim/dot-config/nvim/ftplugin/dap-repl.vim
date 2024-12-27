inoremap <buffer> <c-h> <esc><c-w>h
inoremap <buffer> <c-j> <esc><c-w>j
inoremap <buffer> <c-k> <esc><c-w>k
inoremap <buffer> <c-l> <esc><c-w>l

inoremap <buffer> <F3> <ESC>:lua require'dap'.terminate(); vim.cmd('startinsert!')<CR>
inoremap <buffer> <F5> <ESC>:lua require'dap'.continue(); vim.cmd('startinsert!')<CR>

setlocal cc=-1 nocuc

lua require('dap.ext.autocompl').attach()
