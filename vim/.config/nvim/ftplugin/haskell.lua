vim.lsp.start(vim.tbl_extend('force', require('me.lsp').mk_config(), {
  name = 'hls',
  cmd = {'haskell-language-server-wrapper', '--lsp'},
  on_attach = function(client, bufnr)
    require('lsp_compl').attach(client, bufnr, { server_side_fuzzy_completion = true })
  end,
  init_options = {
    languageServerHaskell = {
      formattingProvider = "ormolu",
    }
  },
}))
