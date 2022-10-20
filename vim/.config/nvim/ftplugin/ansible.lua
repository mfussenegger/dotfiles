local api = vim.api
local opts = { buffer = true, silent = true }
local set = vim.keymap.set
set('v', '<leader>te', function() require('term').run_ansible() end, opts)
set('n', '<leader>te', ":w<CR> :lua require('term').run_ansible()<CR>", opts)

vim.bo.keywordprg = ':sp term://ansible-doc'

local fname = api.nvim_buf_get_name(0)
if fname:find('tasks/') then
  vim.bo.path = vim.bo.path .. ',' .. vim.fs.dirname(fname:gsub("tasks/", "files/"))
end

vim.lsp.start(vim.tbl_extend('force', require('me.lsp').mk_config(), {
  name = 'ansible-ls',
  cmd = {'node', os.getenv('HOME') .. '/dev/ansible/ansible-language-server/out/server/src/server.js', '--stdio'},
  on_attach = function(client, bufnr)
    -- Keep using ansible-doc via keywordprg its content is more detailed
    client.server_capabilities.hoverProvider = false
    require('lsp_compl').attach(client, bufnr)
  end,
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
}))
