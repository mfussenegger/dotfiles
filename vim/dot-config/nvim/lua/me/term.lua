local api = vim.api

local M = {}

local jobid = nil
local winid = nil
local repls = {
  python = "py",
  lua = "lua",
  haskell = "stack ghci",
}


local function launch_term(cmd, opts)
  opts = opts or {}
  vim.cmd("belowright new")
  winid = vim.fn.win_getid()
  api.nvim_win_set_var(0, 'REPL', 1)
  vim.bo.buftype = "nofile"
  vim.bo.bufhidden = "wipe"
  vim.bo.buflisted = false
  vim.bo.swapfile = false
  opts = vim.tbl_extend('error', opts, {
    on_exit = function()
      jobid = nil
    end
  })
  jobid = vim.fn.termopen(cmd, opts)
end

local function close_term()
  if not jobid then
    return
  end
  vim.fn.jobstop(jobid)
  if winid and api.nvim_win_is_valid(winid) then
    api.nvim_win_close(winid, true)
  end
  winid = nil
end


function M.repl()
  local win = api.nvim_get_current_win()
  M.toggle(repls[vim.bo.filetype])
  api.nvim_set_current_win(win)
end


function M.toggle(cmd)
  if jobid then
    close_term()
  else
    cmd = cmd or (vim.fn.environ()['SHELL'] or 'sh')
    launch_term(cmd)
  end
end


function M.run()
  local filepath = api.nvim_buf_get_name(0)
  local lines = api.nvim_buf_get_lines(0, 0, 1, true)
  ---@type string|string[]
  local cmd = filepath
  if not vim.startswith(lines[1], "#!/usr/bin/env") then
    if vim.bo.filetype == "haskell" then
      cmd = {"stack", "run"}
    else
      local choice = vim.fn.confirm(
        "File has no shebang, sure you want to execute it?", "&Yes\n&No")
      if choice ~= 1 then
        return
      end
    end
  end
  local stat = vim.loop.fs_stat(filepath)
  if stat then
    local user_execute = tonumber("00100", 8)
    if bit.band(stat.mode, user_execute) ~= user_execute then
      local newmode = bit.bor(stat.mode, user_execute)
      vim.loop.fs_chmod(filepath, newmode)
    end
  end
  close_term()
  launch_term(cmd)
end


function M.cr8_run_next()
  local bufnr = api.nvim_get_current_buf()
  local parser = vim.treesitter.get_parser(bufnr)
  local tree = parser:parse()[1]
  local root = tree:root()
  local lnum, col = unpack(api.nvim_win_get_cursor(0))
  lnum = lnum - 1
  local cursor_node = root:descendant_for_range(lnum, col, lnum, col)
  if not cursor_node then
    return
  end
  local parent = cursor_node:parent()
  while parent ~= nil do
    local type = parent:type()
    if type == "table" and parent:child_count() > 0 then
      local child = parent:child(1)
      if child and child:type() == "bare_key" then
        local name = vim.treesitter.get_node_text(child, bufnr)
        if name == "setup" or name == "teardown" then
          local cmd = {
            'cr8',
            'run-spec',
            api.nvim_buf_get_name(bufnr),
            'localhost:4200',
            '--action', name,
          }
          close_term()
          launch_term(cmd)
          return
        end
      end
    end
    parent = parent:parent()
  end
  local query = vim.treesitter.query.parse(vim.bo.filetype, [[
    ((table_array_element
      (bare_key) @element_name
      (#eq? @element_name "queries")
      (pair
        (bare_key) @property
        (string) @value
        (#eq? @property "name")
      )
     )
    )
  ]])
  local last = nil
  for id, node in query:iter_captures(root, bufnr, 0, lnum) do
    local capture = query.captures[id]
    if capture == "value" then
      last = node
    end
  end
  if last then
    local name = vim.treesitter.get_node_text(last, bufnr)
    local cmd = {
      'cr8',
      'run-spec',
      api.nvim_buf_get_name(bufnr),
      'localhost:4200',
      '--action', 'queries',
      '--re-name', '^' .. string.sub(name, 2, #name - 1) .. '$'
    }
    close_term()
    launch_term(cmd)
  else
    vim.notify('No query found near cursor with `name` property')
  end
end


function M.cr8_run_file()
  local cmd = {
    'cr8',
    'run-spec',
    api.nvim_buf_get_name(0),
    'localhost:4200',
  }
  close_term()
  launch_term(cmd)
end


function M.send_line(line)
  if not jobid then
    return
  end
  vim.fn.chansend(jobid, line .. '\n')
end


function M.send_selection()
  if not jobid then
    return
  end
  local mode = api.nvim_get_mode()
  local pos1
  local pos2
  local type
  if vim.tbl_contains({"v", "V", ""}, mode.mode) then
    pos1 = vim.fn.getpos("v")
    pos2 = vim.fn.getpos(".")
    type = mode.mode
  else
    pos1 = vim.fn.getpos("'<")
    pos2 = vim.fn.getpos("'>")
    type = vim.fn.visualmode()
    if type == "V" then
      pos2[3] = #vim.fn.getline(pos2[2])
    end
  end
  local lines = vim.fn.getregion(pos1, pos2, { type = type })
  local indent = math.huge
  for _, line in ipairs(lines) do
    indent = math.min(line:find("[^ ]") or math.huge, indent)
  end
  indent = indent == math.huge and 0 or indent
  for _, line in ipairs(lines) do
    vim.fn.chansend(jobid, line:sub(indent) .. '\n')
  end
end


return M
