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

api.nvim_create_user_command("B", function(args)
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
  api.nvim_buf_delete(curbuf, { force = args.bang })
end, { bang = true })

api.nvim_create_user_command("Osv", function()
  require("osv").launch()
end, {})
