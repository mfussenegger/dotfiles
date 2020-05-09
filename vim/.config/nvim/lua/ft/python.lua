local M = {}

local highlights = os.getenv('HOME') .. '/dev/tree-sitter/tree-sitter-python/queries/highlights.scm'
local query = table.concat(vim.fn.readfile(highlights), '\n')

function M.attach()
  vim.treesitter.TSHighlighter.new(query)
end

return M
