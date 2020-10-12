local M = {}

local highlights = os.getenv('HOME') .. '/dev/tree-sitter/tree-sitter-java/queries/highlights.scm'
local query = table.concat(vim.fn.readfile(highlights), '\n')

function M.attach()
  vim.treesitter.highlighter.new(0, 'java', query)
end


return M
