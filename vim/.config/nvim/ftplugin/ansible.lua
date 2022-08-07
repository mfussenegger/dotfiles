local opts = { buffer = true, silent = true }
local set = vim.keymap.set
set('v', '<leader>te', function() require('term').run_ansible() end, opts)
set('n', '<leader>te', ":w<CR> :lua require('term').run_ansible()<CR>", opts)

vim.bo.keywordprg = ':sp term://ansible-doc'
