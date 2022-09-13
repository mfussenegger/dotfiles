local api = vim.api
vim.bo.formatprg = "python -m json.tool"

local bufnr = api.nvim_get_current_buf()
local bufname = api.nvim_buf_get_name(bufnr)
if vim.endswith(bufname, '.vscode/launch.json') then
  api.nvim_create_autocmd('BufWritePost', {
    group = api.nvim_create_augroup('launch-json-' .. tostring(bufnr), { clear = true }),
    buffer = bufnr,
    callback = function()
      require('dap.ext.vscode').load_launchjs()
    end
  })
end
