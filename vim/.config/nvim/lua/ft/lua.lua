local M = {}

function M.attach()
  local query = require('vim.treesitter.query').get_query('lua', 'highlights')
  if query then
    vim.treesitter.highlighter.new(0, 'lua', query)
  end
end

return M
