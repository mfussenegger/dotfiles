local api = vim.api
vim.g.no_python_maps = true
vim.bo.omnifunc = nil

vim.cmd('compiler pyunit')
vim.bo.makeprg = 'python %'

local dap = require('dap')
local dappy = require('dap-python')
if not dap.adapters.python then
  dappy.setup('~/.virtualenvs/tools/bin/python')
end

local silent = { silent = true }

local function save(fn)
  return function()
    if vim.bo.modified then
      vim.cmd.w()
    end
    fn()
  end
end

vim.keymap.set('n', '<leader>dn', save(dappy.test_method), silent)
vim.keymap.set('n', '<leader>dN', function()
  if vim.bo.modified then
    vim.cmd.w()
  end
  local opts = {
    config = {
      justMyCode = false
    }
  }
  dappy.test_method(opts)
end, silent)
vim.keymap.set('n', '<leader>df', save(dappy.test_class), silent)
vim.keymap.set(
  'v', '<leader>ds', '<ESC>:lua require("dap-python").debug_selection()<CR>', silent)


local function run_cmds(cmds)
  return function()
    for _, cmd in pairs(cmds) do
      vim.cmd(cmd)
    end
    vim.cmd('silent e!')
    require("lint").try_lint("ruff")
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
else
  api.nvim_create_autocmd('BufWritePost', {
    callback = function() require("lint").try_lint("ruff") end,
    buffer = bufnr,
    group = api.nvim_create_augroup('ruff-' .. bufnr, {})
  })
end


local lsp = require("me.lsp")
local config = lsp.mk_config {
  name = "pylsp",
  cmd = {"pylsp"},
  cmd_env = {},
  root_dir = lsp.find_root({"setup.py", "setup.cfg", ".git"}),
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
