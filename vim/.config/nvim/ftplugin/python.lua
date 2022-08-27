local api = vim.api

vim.cmd('compiler pyunit')
vim.bo.makeprg = 'python %'

local dap = require('dap')
local dappy = require('dap-python')
if not dap.adapters.python then
  dappy.setup('~/.virtualenvs/tools/bin/python')
end

local function test_runner()
  if vim.loop.fs_stat('manage.py') then
    return 'django'
  else
    return 'unittest'
  end
end


local function test_method()
  return dappy.test_method({ test_runner = test_runner() })
end

local function test_class()
  return dappy.test_class({ test_runner = test_runner() })
end


local silent = { silent = true }
vim.keymap.set('n', '<leader>dn', test_method, silent)
vim.keymap.set('n', '<leader>dN', function()
  local opts = {
    test_runner = test_runner(),
    config = {
      justMyCode = false
    }
  }
  dappy.test_method(opts)
end, silent)
vim.keymap.set('n', '<leader>df', test_class, silent)
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
if next(on_write_cmds) then
  local bufnr = api.nvim_get_current_buf()
  api.nvim_create_autocmd('BufWritePost', {
    callback = run_cmds(on_write_cmds),
    buffer = bufnr,
    group = api.nvim_create_augroup('black-' .. bufnr, {})
  })
end


local lsp = require('me.lsp.conf')
local config = lsp.mk_config()
config.cmd = {'pylsp'}
config.root_dir = require('jdtls.setup').find_root({'.git', 'setup.py', 'setup.cfg'})
config.on_attach = lsp.on_attach
lsp.start(config, {
  reuse_client = function(client, conf)
    return (client.name == conf.name
      and (
        client.config.root_dir == conf.root_dir
        or (conf.root_dir == nil and vim.startswith(api.nvim_buf_get_name(0), "/usr/lib/python"))
      )
    )
  end
})
