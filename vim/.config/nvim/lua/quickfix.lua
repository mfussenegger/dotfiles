local api = vim.api
local M = {}
local win_pre_copen = nil

function M.toggle()
  for _, win in pairs(api.nvim_list_wins()) do
    local buf = api.nvim_win_get_buf(win)
    if api.nvim_buf_get_option(buf, 'buftype') == 'quickfix' then
      api.nvim_command('cclose')
      if win_pre_copen then
        local w = api.nvim_win_get_number(win_pre_copen)
        if api.nvim_win_is_valid(w) then
          api.nvim_set_current_win(w)
        end
        win_pre_copen = nil
      end
      return
    end
  end

  -- no quickfix buffer found so far, so show it
  win_pre_copen = api.nvim_get_current_win()
  api.nvim_command('belowright copen')
end

return M
