local M = {}

function M.attach()
  local query = require('vim.treesitter.query').get_query('python', 'highlights')
  if query then
    vim.treesitter.highlighter.new(0, 'python', query)
  end
end

return M
