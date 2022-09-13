local dap = require('dap')
local api = vim.api
dap.adapters.nlua = function(callback, conf)
  local adapter = {
    type = 'server',
    host = '127.0.0.1',
    port = conf.port
  }
  if conf.start_neovim then
    local start_opts = conf.start_neovim
    conf.start_neovim = nil
    local handle
    local pid_or_err
    local opts = {
      args = {
        '-e',
        vim.v.progpath,
        '-c',
        string.format("lua require('osv').launch({ port = %s })", conf.port),
        start_opts.fname or api.nvim_buf_get_name(0),
      },
      cwd = start_opts.cwd or vim.fn.getcwd(),
      detached = true,
    }
    handle, pid_or_err = vim.loop.spawn('/usr/bin/alacritty', opts, function(code)
      handle:close()
      assert(code == 0, "Exit code must be 0, not " .. tostring(code))
    end)
    if not handle then
      error(pid_or_err)
    end
    local timer = vim.loop.new_timer()
    timer:start(1000, 0, function()
      timer:stop()
      timer:close()
      vim.schedule(function()
        callback(adapter)
      end)
    end)
  else
    callback(adapter)
  end
end


local function free_port()
  local tcp = vim.loop.new_tcp()
  tcp:bind('127.0.0.1', 0)
  local port = tcp:getsockname().port
  tcp:shutdown()
  tcp:close()
  return port
end


dap.configurations.lua = {
  {
    type = 'nlua',
    request = 'attach',
    name = 'Attach',
    port = function()
      return assert(tonumber(vim.fn.input('Port: ')), 'Port is required')
    end
  },
  {
    type = "nlua",
    request = "attach",
    name = "New instance (current file)",
    port = free_port,
    start_neovim = {}
  },
  {
    type = "nlua",
    request = "attach",
    name = "New instance (crate/crate)",
    port = free_port,
    start_neovim = {
      cwd = os.getenv('HOME') .. '/dev/crate/crate',
      fname = 'server/src/test/java/io/crate/planner/PlannerTest.java',
    }
  }
}

if vim.loop.fs_stat(".stylua.toml") then
  vim.bo.formatprg = "stylua -"
end


local lsp = require('me.lsp')
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
config.root_dir = vim.fs.dirname(vim.fs.find({'.git'}, { upward = true })[1])
vim.lsp.start(config)

local bufnr = api.nvim_get_current_buf()

if vim.loop.fs_stat("lemmy.sh") then
  local group = vim.api.nvim_create_augroup('lemmy-' .. bufnr, { clear = true })
  api.nvim_create_autocmd('BufWritePost', {
    command = 'silent !./lemmy.sh',
    buffer = bufnr,
    group = group
  })
end
