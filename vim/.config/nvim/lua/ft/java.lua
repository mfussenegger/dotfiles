local M = {}

local highlights = os.getenv('HOME') .. '/dev/tree-sitter/tree-sitter-java/queries/highlights.scm'
local query = table.concat(vim.fn.readfile(highlights), '\n')

function M.attach()
  vim.treesitter.highlighter.new(query)
end

return M
