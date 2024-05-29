vim.lsp.start(require('me.lsp').mk_config {
  name = 'hls',
  cmd = {'haskell-language-server-wrapper', '--lsp'},
  root_dir = vim.fs.root(0, {"stack.yml", ".git"}),
  on_attach = function(client, bufnr)
    if vim.lsp.completion then
      vim.lsp.completion.enable(true, client.id, bufnr, { autotrigger = true })
    else
      require('lsp_compl').attach(client, bufnr, {
        server_side_fuzzy_completion = true
      })
    end
  end,
  init_options = {
    languageServerHaskell = {
      formattingProvider = "ormolu",
    }
  },
})
