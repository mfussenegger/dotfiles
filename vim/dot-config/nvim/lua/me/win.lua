local a = vim.api
local M = {}


local function close_and_new(cmd)
  local winnr = a.nvim_get_current_win()
  local config = a.nvim_win_get_config(winnr)
  vim.cmd(cmd)
  if config.relative and config.relative ~= '' then
    a.nvim_win_close(winnr, true)
  end
end

function M.split()
  close_and_new('split')
end

function M.vsplit()
  close_and_new('vsplit')
end


function M.goto_def()
  local winnr = a.nvim_get_current_win()
  local config = a.nvim_win_get_config(winnr)
  vim.cmd('split')
  a.nvim_feedkeys('', 'n', true)
  if config.relative and config.relative ~= '' then
    a.nvim_win_close(winnr, true)
  end
end

return M
