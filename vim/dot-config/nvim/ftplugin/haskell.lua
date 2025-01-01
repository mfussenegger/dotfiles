vim.lsp.start(require('me.lsp').mk_config {
  name = 'hls',
  cmd = {'haskell-language-server-wrapper', '--lsp'},
  root_dir = vim.fs.root(0, {"stack.yml", ".git"}),

  ---@param client vim.lsp.Client
  on_attach = function(client)
    client.server_capabilities.semanticTokensProvider = nil
  end,
  settings = {
    haskell = {
      formattingProvider = "ormolu",
    }
  },
})
