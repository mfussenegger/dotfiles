local lsp = require('me.lsp.conf')
local config = lsp.mk_config()

config.settings = {
  Lua = {
    diagnostics = {
      globals = {'vim', 'it', 'describe'}
    },
    runtime = {
      version = "LuaJIT",
    },
    workspace = {
      library = vim.api.nvim_get_runtime_file("", true),
    },
    telemetry = {
      enable = false,
    },
  }
}
config.name = 'luals'
config.cmd = {'lua-language-server'}
config.root_dir = require('jdtls.setup').find_root({'.git'})
config.on_attach = function(client, bufnr)
  lsp.on_attach(client, bufnr)
  if vim.loop.fs_stat(".stylua.toml") then
    vim.bo.formatprg = "stylua -"
    vim.keymap.del('n', 'gq', { buffer = bufnr })
    vim.keymap.del('v', 'gq', { buffer = bufnr })
    local group = vim.api.nvim_create_augroup('format-' .. bufnr, {})
    vim.api.nvim_create_autocmd('BufWritePost', {
      callback = function()
        vim.cmd('silent !stylua %')
        vim.cmd('silent e!')
      end,
      buffer = bufnr,
      group = group
    })
  end
end
lsp.start(config)
