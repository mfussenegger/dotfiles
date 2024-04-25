local opts = { buffer = true, silent = true }
local set = vim.keymap.set
set('v', '<leader>te', function() require('ansible').run() end, opts)
set('n', '<leader>te', ":w<CR> :lua require('ansible').run()<CR>", opts)

-- ansible-language-server shows a message request with a `ENOENT` error
-- if the file does not exist on disk. Ensure it does
if vim.bo.buftype ==  "" then
  local stat = vim.loop.fs_stat(vim.api.nvim_buf_get_name(0))
  if not stat then
    vim.fn.mkdir(vim.fn.expand("%:p:h"), "p")
    vim.cmd("w")
  end
end

local dap = require("dap")
if not dap.adapters.ansible then
  dap.adapters.ansible = {
    type = "executable",
    command = os.getenv("HOME") .. "/.virtualenvs/ansibug/bin/python",
    args = { "-m", "ansibug", "dap" },
  }
  dap.configurations["yaml.ansible"] = {
    {
      type = "ansible",
      request = "launch",
      name = "Debug playbook",
      playbook = "${file}"
    }
  }
end

vim.lsp.start(require('me.lsp').mk_config {
  name = 'ansible-ls',
  cmd = {'ansible-language-server', '--stdio'},
  root_dir = require("me.lsp").find_root({".git"}),
  settings = {
    ansible = {
      ansible = {
        path = '/usr/bin/ansible',
        useFullyQualifiedCollectionNames = true,
      },
      python = {
        interpreterPath = '/usr/bin/python',
      },
      executionEnvironment = {
        enabled = false,
      },
      validation = {
        lint = {
          enabled = false,
        }
      },
      completion = {
        provideRedirectModules = true,
        provideModuleOptionAliases = true,
      }
    }
  }
})
