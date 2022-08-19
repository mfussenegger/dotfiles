local api = vim.api
vim.cmd [[
  source ~/.config/nvim/options.vim
  source ~/.config/nvim/mappings.vim
  source ~/.config/nvim/plugin_options.vim
]]

vim.g.python3_host_prog = vim.fn.expand('$HOME/.virtualenvs/nvim/bin/python')

local keymap = vim.keymap
local accept_compl_or_cr = function()
  return require('lsp_compl').accept_pum() and '<c-y>' or '<CR>'
end
keymap.set('i', '<CR>', accept_compl_or_cr, { expr = true })

keymap.set('n', ']q', ':cnext<CR>')
keymap.set('n', '[q', ':cprevious<CR>')
keymap.set('n', ']Q', ':cfirst<CR>')
keymap.set('n', '[Q', ':clast<CR>')
keymap.set('n', ']l', ':lnext<CR>')
keymap.set('n', '[l', ':lprevious<CR>')
keymap.set('n', ']L', ':lfirst<CR>')
keymap.set('n', '[L', ':llast<CR>')

keymap.set('n', ']v', function() require('me.lsp.ext').next_highlight() end)
keymap.set('n', '[v', function() require('me.lsp.ext').prev_highlight() end)

require('me').setup()
require('me.fzy').setup()
require('me.dap').setup()
do
  local lint = require('lint')
  lint.linters_by_ft = {
    markdown = {'vale'},
    rst = {'vale'},
    java = {'codespell'},
    lua = {'codespell', 'luacheck'},
    sh = {'shellcheck'},
    ['yaml.ansible'] = {'ansible_lint'},
    yaml = {'yamllint'},
    gitcommit = {'codespell'},
  }
  api.nvim_create_autocmd({'BufWritePost', 'BufEnter', 'BufLeave'}, {
    group = api.nvim_create_augroup('lint', { clear = true }),
    callback = function() lint.try_lint() end,
  })
end


vim.o.scrollback=100000

api.nvim_create_user_command('Grep', 'silent grep! <args> | copen | wincmd p', { nargs = '+' })

