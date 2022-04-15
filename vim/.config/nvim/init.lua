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

require('me.fzy').setup()
require('me.dap').setup()


vim.o.scrollback=100000
