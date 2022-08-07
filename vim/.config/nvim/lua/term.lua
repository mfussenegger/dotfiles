local api = vim.api

local M = {}
local jobid = nil
local winid = nil


local function bo(option, value)
    api.nvim_buf_set_option(0, option, value)
end

local function launch_term(cmd, opts)
  opts = opts or {}
  vim.cmd("belowright new")
  winid = vim.fn.win_getid()
  api.nvim_win_set_var(0, 'REPL', 1)
  bo('buftype', 'nofile')
  bo('bufhidden', 'wipe')
  bo('buflisted', false)
  bo('swapfile', false)
  opts = vim.tbl_extend('error', opts, {
    on_exit = function()
      jobid = nil
    end
  })
  jobid = vim.fn.termopen(cmd, opts)
end

local function close_term()
    if not jobid then return end
    vim.fn.jobstop(jobid)
    if api.nvim_win_is_valid(winid) then
        api.nvim_win_close(winid, true)
    end
    winid = nil
end

function M.toggle()
    if jobid then
        close_term()
    else
        launch_term(vim.fn.environ()['SHELL'] or 'sh')
    end
end


function M.run()
    local filepath = api.nvim_buf_get_name(0)
    close_term()
    launch_term(filepath)
end


local function get_selected_lines()
  -- [bufnr, lnum, col, off]; 1-indexed
  local start = vim.fn.getpos('v')
  local end_ = vim.fn.getpos('.')
  local start_row = start[2]
  local start_col = start[3]
  local end_row = end_[2]
  local end_col = end_[3]
  if start_row == end_row and end_col < start_col then
    end_col, start_col = start_col, end_col
  elseif end_row < start_row then
    start_row, end_row = end_row, start_row
    start_col, end_col = end_col, start_col
  end
  return api.nvim_buf_get_text(0, start_row - 1, start_col - 1, end_row - 1, end_col - 1, {})
end


function M.run_ansible()
  local path = vim.fn.fnamemodify(api.nvim_buf_get_name(0), ':p')
  local role_match = path:match('/roles/([%w-]+)/')
  if role_match then
    local mode = api.nvim_get_mode().mode
    if mode == 'v' or mode == 'V' then
      local lines = get_selected_lines()
      local tmptask = string.gsub(path, role_match, 'tmptask')
      tmptask = vim.fn.fnamemodify(tmptask, ':h') .. '/main.yml'
      vim.fn.mkdir(vim.fn.fnamemodify(tmptask, ':h'), 'p')
      local f = io.open(tmptask, "w")
      assert(f, "Must be able to open tmpfile in write mode")
      f:write(table.concat(lines, '\n'))
      f:flush()
      f:close()
      role_match = 'tmptask'
    end
    local _, end_ = path:find('/playbooks/')
    local playbook_dir = nil
    if end_ then
      playbook_dir = path:sub(1, end_)
    end
    local cmd = {
      'ansible',
      'localhost',
      '--playbook-dir', playbook_dir,
      '-m', 'import_role',
      '-a', 'name=' .. role_match
    }
    close_term()
    launch_term(cmd)
  elseif path:match('/playbooks/') then
    local cmd = {'ansible-playbook', path}
    close_term()
    launch_term(cmd)
  end
end


function M.sendLine(line)
    if not jobid then return end
    vim.fn.chansend(jobid, line .. '\n')
end

function M.sendSelection()
    if not jobid then return end
    local start_row, start_col = unpack(api.nvim_buf_get_mark(0, '<'))
    local end_row, end_col = unpack(api.nvim_buf_get_mark(0, '>'))
    local mode = vim.fn.visualmode()
    local offset
    if mode == '' then -- in block mode all following lines are indented
      offset = start_col
    elseif mode == 'V' then
      end_row = end_row + 1
      offset = 0
    else
      offset = 0
    end
    local lines = api.nvim_buf_get_lines(0, start_row - 1, end_row, false)
    for idx, line in pairs(lines) do
      local l
      if idx == 1 and idx == #lines then
        l = line:sub(start_col + 1, end_col + 1)
      elseif idx == 1 then
        l = line:sub(start_col + 1)
      elseif idx == #lines then
        l = line:sub(offset + 1, end_col > #line and #line or end_col + 1)
      elseif offset > 0 then
        l = line:sub(offset + 1)
      else
        l = line
      end
      vim.fn.chansend(jobid, l .. '\n')
    end
end

return M
