local api = vim.api
local M = {}

local function exit()
  local key = api.nvim_replace_termcodes("<C-j>", true, false, true)
  api.nvim_feedkeys(key, 'n', true)
end


function M.maybe()
  local expandable = vim.fn['vsnip#expandable']()
  if expandable ~= 0 then
    vim.fn['vsnip#expand']()
  else
    local clients = vim.lsp.get_active_clients()
    if not next(clients) then
      return exit()
    end
    local params = vim.lsp.util.make_position_params()
    local results, err = vim.lsp.buf_request_sync(0, 'textDocument/completion', params, 1000)
    assert(not err, vim.inspect(err))
    for _, resp in pairs(results) do
      local result = resp.result
      if not result then return end
      local mode = api.nvim_get_mode()['mode']
      if mode ~= 'i' and mode ~= 'ic' then
        return
      end
      local items = result.items
      if #items == 0 then
        return exit()
      end
      local matches = {}
      for _, item in pairs(items) do
        local kind = vim.lsp.protocol.CompletionItemKind[item.kind] or ''
        if kind == 'Snippet' then
          table.insert(matches, {
            word = item.label,
            abbr = item.label,
            kind = 'Snippet',
            menu = item.detail or '',
            icase = 1,
            dup = 1,
            empty = 1,
            user_data = item
          })
        end
      end
      if #matches == 0 then
        return exit()
      end
      local cursor_pos = api.nvim_win_get_cursor(0)[2]
      local line = api.nvim_get_current_line()
      local line_to_cursor = line:sub(1, cursor_pos)
      local col = vim.fn.match(line_to_cursor, '\\k*$')
      vim.fn.complete(col + 1, matches)
      if #matches == 1 then
        api.nvim_feedkeys(
          api.nvim_replace_termcodes("<C-n>", true, false, true), 'n', true)
        api.nvim_feedkeys(
          api.nvim_replace_termcodes("<CR>", true, false, true), 'm', true)
      end
    end
  end
end

return M
