local M = {}
local api = vim.api

local function any(items, predicate)
  for _, item in pairs(items) do
    if predicate(item) then
      return true
    end
  end
  return false
end


local function has_severity(severity)
  return function(diagnostic)
    return diagnostic.severity == severity
  end
end


function M.set_loclist(bufnr, diagnostics)
  local win = api.nvim_get_current_win()
  local cur_buf = api.nvim_win_get_buf(win)
  if bufnr ~= cur_buf then
    return
  end
  if any(diagnostics, has_severity(vim.diagnostic.severity.ERROR)) then
    diagnostics = vim.tbl_filter(has_severity(vim.diagnostic.severity.ERROR), diagnostics)
  elseif any(diagnostics, has_severity(vim.diagnostic.severity.WARN)) then
    diagnostics = vim.tbl_filter(has_severity(vim.diagnostic.severity.WARN), diagnostics)
  end
  vim.fn.setloclist(0, {}, 'r', {
    title = 'Language Server',
    items = vim.diagnostic.toqflist(diagnostics)
  })
end


return M
