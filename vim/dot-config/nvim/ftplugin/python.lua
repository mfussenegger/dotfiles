local api = vim.api
vim.g.no_python_maps = true
vim.bo.omnifunc = nil

vim.cmd('compiler pyunit')
vim.bo.makeprg = 'python %'

local dap = require('dap')
local dappy = require('dap-python')
if not dap.adapters.python then
  dappy.setup('~/.virtualenvs/tools/bin/python', {
  })

  local django = dappy.test_runners.django
  dappy.test_runners.django = function(classnames, methodname)
    local name, args = django(classnames, methodname)
    table.insert(args, 2, "--timing")
    table.insert(args, 2, "--keepdb")
    return name, args
  end
end

local silent = { silent = true }

local function save(fn, ...)
  local args = {...}
  return function()
    if vim.bo.modified then
      vim.cmd.w()
    end
    fn(unpack(args))
  end
end

local opts = {
  config = {
    autoReload = {
      enabled = true
    }
  }
}
vim.keymap.set('n', '<leader>dn', save(dappy.test_method, opts), silent)
vim.keymap.set('n', '<leader>dN', save(dappy.test_method, {config = { justMyCode = false }}), silent)
vim.keymap.set('n', '<leader>df', save(dappy.test_class), silent)
vim.keymap.set(
  'v', '<leader>ds', '<ESC>:lua require("dap-python").debug_selection()<CR>', silent)


local function run_cmds(cmds)
  return function()
    for _, cmd in pairs(cmds) do
      vim.cmd(cmd)
    end
    vim.cmd('silent e!')
  end
end


local on_write_cmds = {}
if vim.fn.executable('isort') == 1 then
  table.insert(on_write_cmds, 'silent !isort --profile black %')
end
if vim.fn.executable('black') == 1 then
  table.insert(on_write_cmds, 'silent !black -q %')
end
local bufnr = api.nvim_get_current_buf()
if next(on_write_cmds) then
  api.nvim_create_autocmd('BufWritePost', {
    callback = run_cmds(on_write_cmds),
    buffer = bufnr,
    group = api.nvim_create_augroup('black-' .. bufnr, {})
  })
end


local lsp = require("me.lsp")
local config = lsp.mk_config {
  name = "pylsp",
  cmd = {"pylsp"},
  cmd_env = {},
  root_dir = vim.fs.root(0, {"setup.py", "setup.cfg", ".git"}),
}
if config.root_dir then
  local folders = {"venv", ".venv", "env", ".env"}
  for _, folder in ipairs(folders) do
    local path = config.root_dir .. "/" .. folder
    local stat = vim.loop.fs_stat(path)
    if stat then
      config.cmd_env["VIRTUAL_ENV"] = path
    end
  end
end
vim.lsp.start(config, {
  bufnr = bufnr,
  reuse_client = function(client, conf)
    return (client.name == conf.name
      and (
        client.config.root_dir == conf.root_dir
        or (
          conf.root_dir == nil
          and vim.startswith(api.nvim_buf_get_name(0), "/usr/lib/python")
        )
      )
    )
  end
})
if vim.bo.buftype == "" and vim.fn.filereadable(api.nvim_buf_get_name(0)) == 1 then
  vim.lsp.start(lsp.mk_config {
    cmd = {"ruff", "server"},
    name = "ruff",
    root_dir = config.root_dir,

    on_attach = function()
    end,

    ---@param client vim.lsp.Client
    on_init = function(client)
      client.server_capabilities.hoverProvider = nil
    end,
  })
end
