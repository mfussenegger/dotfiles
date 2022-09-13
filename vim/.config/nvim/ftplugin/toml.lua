local config = vim.tbl_extend('force', require('me.lsp').mk_config(), {
  cmd = {'taplo', 'lsp', 'stdio'},
  init_options = {
    configurationSection = "evenBetterToml",
    cachePath = vim.NIL
  },
  root_dir = vim.fs.dirname(vim.fs.find({'taplo.toml', '.taplo.toml'}, { upward = true })[1])
})
vim.lsp.start(config)

local fname = vim.api.nvim_buf_get_name(0)
if string.match(fname, ".*/specs/.*") then
  local opts = { buffer = true, silent = true }
  local set = vim.keymap.set
  set('n', '<leader>dn', function() require('term').cr8_run_next() end, opts)
  set('n', '<leader>df', function() require('term').cr8_run_file() end, opts)
end
