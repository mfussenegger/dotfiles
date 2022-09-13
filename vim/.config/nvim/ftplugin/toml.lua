local config = vim.tbl_extend('force', require('me.lsp').mk_config(), {
  cmd = {'taplo', 'lsp', 'stdio'},
  init_options = {
    configurationSection = "evenBetterToml",
    cachePath = vim.NIL
  },
  root_dir = vim.fs.dirname(vim.fs.find({'taplo.toml', '.taplo.toml'}, { upward = true })[1])
})
vim.lsp.start(config)
