local M = {}

function M.attach()
  local query = require('vim.treesitter.query').get_query('java', 'highlights')
  if query then
    vim.treesitter.highlighter.new(0, 'java', query)
  end
end


return M
