inoremap <buffer> <c-h> <esc><c-w>h
inoremap <buffer> <c-j> <esc><c-w>j
inoremap <buffer> <c-k> <esc><c-w>k
inoremap <buffer> <c-l> <esc><c-w>l

inoremap <buffer> <F5> <ESC>:lua require'dap'.continue(); vim.cmd('startinsert!')<CR>
inoremap <buffer> <F10> <ESC>:lua require'dap'.step_over(); vim.cmd('startinsert!')<CR>
inoremap <buffer> <F11> <ESC>:lua require'dap'.step_into(); vim.cmd('startinsert!')<CR>
inoremap <buffer> <F12> <ESC>:lua require'dap'.step_out(); vim.cmd('startinsert!')<CR>

setlocal nonumber norelativenumber cc=-1 nocuc
setlocal tagfunc=v:lua.require'me.lsp.ext'.symbol_tagfunc

lua require('dap.ext.autocompl').attach()
