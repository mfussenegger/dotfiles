local opts = { buffer = true, silent = true }
local set = vim.keymap.set
set('v', '<leader>te', function() require('ansible').run() end, opts)
set('n', '<leader>te', ":w<CR> :lua require('ansible').run()<CR>", opts)


vim.lsp.start(vim.tbl_extend('force', require('me.lsp').mk_config(), {
  name = 'ansible-ls',
  cmd = {'node', os.getenv('HOME') .. '/dev/ansible/ansible-language-server/out/server/src/server.js', '--stdio'},
  on_attach = function(client, bufnr)
    -- Keep using ansible-doc via keywordprg its content is more detailed
    vim.keymap.del('n', 'K', { buffer = bufnr })
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
