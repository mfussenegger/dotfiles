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

vim.lsp.start(require('me.lsp').mk_config {
  name = 'ansible-ls',
  cmd = {'ansible-language-server', '--stdio'},
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
})
