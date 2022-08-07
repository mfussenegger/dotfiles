local M = {}
local lsp = vim.lsp
local api = vim.api

local function mk_tag_item(name, range, uri)
  local start = range.start
  return {
    name = name,
    filename = vim.uri_to_fname(uri),
    cmd = string.format(
      'call cursor(%d, %d)', start.line + 1, start.character + 1
    )
  }
end

function M.symbol_tagfunc(pattern, flags)
  if not (flags == 'c' or flags == '' or flags == 'i') then
    return vim.NIL
  end
  local clients = vim.lsp.get_active_clients()
  local num_clients = vim.tbl_count(clients)
  local results = {}
  for _, client in pairs(clients) do
    client.request('workspace/symbol', { query = pattern }, function(_, method_or_result, result_or_ctx)
      local result = type(method_or_result) == 'string' and result_or_ctx or method_or_result
      for _, symbol in pairs(result or {}) do
        local loc = symbol.location
        local item = mk_tag_item(symbol.name, loc.range, loc.uri)
        item.kind = (lsp.protocol.SymbolKind[symbol.kind] or 'Unknown')[1]
        table.insert(results, item)
      end
      num_clients = num_clients - 1
    end)
  end
  vim.wait(1500, function() return num_clients == 0 end)
  return results
end



function M.remove_unused_imports()
  vim.diagnostic.setqflist { severity = vim.diagnostic.severity.WARN }
  vim.cmd('packadd cfilter')
  vim.cmd('Cfilter /The import/')
  vim.cmd('cdo normal dd')
  vim.cmd('cclose')
  vim.cmd('wa')
end


local function is_before(x, y)
  if x.start.line < y.start.line then
    return true
  elseif x.start.line == y.start.line then
    return x.start.character < y.start.character
  else
    return false
  end
end

local function move_to_highlight(is_closer)
  local params = lsp.util.make_position_params()
  local win = api.nvim_get_current_win()
  local lnum, col = unpack(api.nvim_win_get_cursor(win))
  lnum = lnum - 1
  local cursor = {
    start = { line = lnum, character = col }
  }
  lsp.buf_request(0, 'textDocument/documentHighlight', params, function(err, result)
    assert(not err, err and err.message)
    local closest = nil
    for _, highlight in pairs(result or {}) do
      local range = highlight.range
      if is_closer(cursor, range) and (closest == nil or is_closer(range, closest)) then
        closest = range
      end
    end
    if closest then
      api.nvim_win_set_cursor(win, { closest.start.line + 1, closest.start.character })
    end
  end)
end

function M.next_highlight()
  return move_to_highlight(is_before)
end


function M.prev_highlight()
  return move_to_highlight(function(x, y) return is_before(y, x) end)
end

return M
