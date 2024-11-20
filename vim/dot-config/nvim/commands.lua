local api = vim.api

api.nvim_create_user_command("Del", function(args)
  local bufnr = api.nvim_get_current_buf()
  local fname = api.nvim_buf_get_name(bufnr)
  if vim.bo[bufnr].buftype == "" then
    local ok, err = os.remove(fname)
    assert(args.bang or ok, err)
  end
  api.nvim_buf_delete(bufnr, { force = args.bang })
end, { bang = true })

api.nvim_create_user_command("GV", function(args)
    local fname = api.nvim_buf_get_name(0)
    local cmd = {
      'G log --date=short --format="%cd %h%d %s (%an)"',
      string.format("-L%d,%d:%s", args.line1, args.line2, fname),
    }
    vim.cmd(table.concat(cmd, " "))
  end, { range = "%" }
)

api.nvim_create_user_command("C", function(args)
  local curbuf = api.nvim_get_current_buf()
  local altbuf = nil
  for _, buf in ipairs(api.nvim_list_bufs()) do
    if buf ~= curbuf and api.nvim_buf_is_loaded(buf) and vim.bo[buf].buftype == "" then
      altbuf = buf
      break
    end
  end
  if not altbuf then
    altbuf = api.nvim_create_buf(true, true)
  end
  for _, win in ipairs(api.nvim_list_wins()) do
    if api.nvim_win_get_buf(win) == curbuf then
      api.nvim_win_set_buf(win, altbuf)
    end
  end
  local force = (args.bang
    or vim.bo[curbuf].buftype == "prompt"
    or api.nvim_buf_get_name(0) == ""
  )
  api.nvim_buf_delete(curbuf, { force = force })
end, { bang = true })

api.nvim_create_user_command("Osv", function(args)
  require("osv").launch({ port = tonumber(args.args) })
end, { nargs = "?" })


api.nvim_create_user_command("Lua", function(args)
  local srcbuf = api.nvim_get_current_buf()
  vim.cmd.split()
  local bufnr = api.nvim_create_buf(true, true)
  api.nvim_buf_set_name(bufnr, "lua://" .. tostring(bufnr))
  if args.range ~= 0 then
    local lines = api.nvim_buf_get_lines(srcbuf, args.line1 -1 , args.line2, true)
    local indent = math.huge
    for _, line in ipairs(lines) do
      indent = math.min(line:find("[^ ]") or math.huge, indent)
    end
    if indent ~= math.huge and indent > 0 then
      for i, line in ipairs(lines) do
        lines[i] = line:sub(indent)
      end
    end
    api.nvim_buf_set_lines(bufnr, 0, -1, true, lines)
    vim.bo[bufnr].modified = false
  end
  vim.bo[bufnr].buftype = "acwrite"
  vim.bo[bufnr].bufhidden = "wipe"
  vim.bo[bufnr].filetype = "lua"
  local function execlua(buf)
    local lines = api.nvim_buf_get_lines(buf, 0, -1, true)
    local code = table.concat(lines, "\n")
    local fn, err = loadstring(code)
    if fn then
      local result = fn()
      if result then
        vim.print(result)
      end
    elseif err then
      vim.notify(err, vim.log.levels.WARN)
    end
  end
  api.nvim_set_current_buf(bufnr)
  api.nvim_create_autocmd("BufWriteCmd", {
    buffer = bufnr,
    callback = function(write_args)
      vim.bo[write_args.buf].modified = false
      execlua(write_args.buf)
    end,
  })
end, { range = "%" })


api.nvim_create_user_command("CUniqueFile", function()
  local qflist = vim.fn.getqflist()
  local result = {}
  local seen = {}
  for _, item in ipairs(qflist) do
    if seen[item.bufnr] == nil then
      seen[item.bufnr] = true
      table.insert(result, item)
    end
  end
  vim.fn.setqflist(qflist, "r")
end, { nargs = 0 })


api.nvim_create_user_command("Term", function(args)
  local srcbuf = api.nvim_get_current_buf()
  local lines
  if args.range ~= 0 then
    lines = api.nvim_buf_get_lines(srcbuf, args.line1 -1 , args.line2, true)
    local indent = math.huge
    for _, line in ipairs(lines) do
      indent = math.min(line:find("[^ ]") or math.huge, indent)
    end
    if indent ~= math.huge and indent > 0 then
      for i, line in ipairs(lines) do
        lines[i] = line:sub(indent)
      end
    end
  end
  vim.cmd("belowright split")
  local win = api.nvim_get_current_win()
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].signcolumn = "no"
  local bufnr = api.nvim_create_buf(true, true)
  api.nvim_win_set_buf(win, bufnr)
  local jobid
  local chan = api.nvim_open_term(bufnr, {
    on_input = function(_, _, _, data)
      pcall(api.nvim_chan_send, jobid, data)
    end
  })
  local opts = {
    height = api.nvim_win_get_height(win),
    width = api.nvim_win_get_width(win),
    pty = true,
    on_stdout = function(_, data)
      local count = #data
      for idx, line in ipairs(data) do
        local islast = idx == count
        local msg = islast and line or (line .. '\n')
        local send_ok = pcall(api.nvim_chan_send, chan, msg)
        if not send_ok then
          return
        end
      end
    end,
    on_exit = function(_, exit_code)
      pcall(api.nvim_chan_send, chan, '\r\n[Process exited ' .. tostring(exit_code) .. ']')
      pcall(api.nvim_buf_set_keymap, bufnr, "t", "<CR>", "<cmd>bd!<CR>", { noremap = true, silent = true})
    end,
  }
  local cmd = args.args == "" and vim.o.shell or args.args
  if lines then
    local inputs = vim.fn.tempname()
    vim.fn.system(string.format('mkfifo "%s"', inputs))
    jobid = vim.fn.jobstart(string.format('cat "%s" | %s', inputs, cmd), opts)
    local f = io.open(inputs, "a")
    if f then
      for _, line in ipairs(lines) do
        f:write(line)
        f:write("\n")
      end
      f:flush()
      f:close()
    end
  else
    jobid = vim.fn.jobstart(cmd, opts)
  end
end, { nargs = "*", range = "%", desc = "Like :term but supporting range" })
