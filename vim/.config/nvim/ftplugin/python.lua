vim.cmd('compiler pyunit')
vim.bo.makeprg = 'python %'

require('dap-python').setup('~/.virtualenvs/tools/bin/python')
local silent = { silent = true }
vim.keymap.set(
  'n', '<leader>dn', function() require('dap-python').test_method() end, silent)
vim.keymap.set(
  'n', '<leader>df', function() require('dap-python').test_class() end, silent)
vim.keymap.set(
  'v', '<leader>ds', '<ESC>:lua require("dap-python").debug_selection()<CR>', silent)
