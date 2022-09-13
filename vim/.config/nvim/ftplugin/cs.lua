-- preload parser to make `get_parser(0)` calls work (`cs` filetype != `c_sharp` parser name)
vim.treesitter.get_parser(0, 'c_sharp')

vim.lsp.start(vim.tbl_extend('force', require('me.lsp').mk_config(), {
  cmd = { 'OmniSharp', '-z', '--hostPID', tostring(vim.fn.getpid()), '--languageserver' },
  name = 'OmniSharp',
  root_dir = vim.fn.getcwd(),
}))
