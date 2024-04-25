local api = vim.api
local M = {}
local win_pre_copen = nil

function M.toggle()
  for _, win in pairs(api.nvim_list_wins()) do
    local buf = api.nvim_win_get_buf(win)
    if api.nvim_buf_get_option(buf, 'buftype') == 'quickfix' then
      api.nvim_command('cclose')
      if win_pre_copen then
        local ok, w = pcall(api.nvim_win_get_number, win_pre_copen)
        if ok and api.nvim_win_is_valid(w) then
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


local efms = {
  python = [[%C %.%#,%A  File "%f"\, line %l%.%#,%Z%[%^ ]%\@=%m]],
}

function M.load()
  local buf = api.nvim_get_current_buf()
  local ft = vim.b[buf]['dap-type'] or vim.bo.filetype
  local efm = efms[ft] or vim.bo.errorformat or vim.g.errorformat
  local lines = api.nvim_buf_get_lines(buf, 0, -1, true)
  vim.fn.setqflist({}, 'r', { efm = efm, lines = lines })
  vim.cmd('belowright copen')
end


return M
