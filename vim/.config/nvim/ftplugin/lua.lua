local lsp = require('me.lsp.conf')
local config = lsp.mk_config()
local library = {}
local path = vim.split(package.path, ";")
table.insert(path, "lua/?.lua")
table.insert(path, "lua/?/init.lua")

local function add(lib)
  for _, p in pairs(vim.fn.expand(lib, false, true)) do
    p = vim.loop.fs_realpath(p)
    library[p] = true
  end
end

add("$VIMRUNTIME")
add("~/.config/nvim")
add("~/.config/nvim/pack/plugins/start/*")
config.settings = {
  Lua = {
    diagnostics = {
      globals = {'vim', 'it', 'describe'}
    },
    runtime = {
      version = "LuaJIT",
      path = path,
    },
    workspace = {
      library = library,
    },
    telemetry = {
      enable = false,
    },
  }
}
local server_dir = vim.fn.expand('~/dev/sumneko/lua-language-server/')
config.name = 'luals'
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
local cmd = {server_dir .. 'bin/lua-language-server', server_dir .. 'main.lua'}
lsp.add_client(cmd, config)
