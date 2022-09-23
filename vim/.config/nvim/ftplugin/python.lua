local api = vim.api

vim.cmd('compiler pyunit')
vim.bo.makeprg = 'python %'

local dap = require('dap')
local dappy = require('dap-python')
if not dap.adapters.python then
  dappy.setup('~/.virtualenvs/tools/bin/python')
end

local silent = { silent = true }
vim.keymap.set('n', '<leader>dn', dappy.test_method, silent)
vim.keymap.set('n', '<leader>dN', function()
  local opts = {
    config = {
      justMyCode = false
    }
  }
  dappy.test_method(opts)
end, silent)
vim.keymap.set('n', '<leader>df', dappy.test_class, silent)
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


local config = require('me.lsp').mk_config()
config.cmd = {'pylsp'}
config.root_dir = vim.fs.dirname(vim.fs.find({'.git', 'setup.py', 'setup.cfg'})[1])
vim.lsp.start(config, {
  reuse_client = function(client, conf)
    return (client.name == conf.name
      and (
        client.config.root_dir == conf.root_dir
        or (conf.root_dir == nil and vim.startswith(api.nvim_buf_get_name(0), "/usr/lib/python"))
      )
    )
  end
})
