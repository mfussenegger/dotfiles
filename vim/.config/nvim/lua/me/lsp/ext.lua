local M = {}
local lsp = vim.lsp

do
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

  local function query_definition(pattern)
    local params = lsp.util.make_position_params()
    local results_by_client, err = lsp.buf_request_sync(0, 'textDocument/definition', params, 1000)
    assert(not err, vim.inspect(err))
    local results = {}
    local add = function(range, uri) table.insert(results, mk_tag_item(pattern, range, uri)) end
    for _, lsp_results in pairs(results_by_client) do
      local result = lsp_results.result or {}
      if result.range then              -- Location
        add(result.range, result.uri)
      else                              -- Location[] or LocationLink[]
        for _, item in pairs(result) do
          if item.range then            -- Location
            add(item.range, item.uri)
          else                          -- LocationLink
            add(item.targetSelectionRange, item.targetUri)
          end
        end
      end
    end
    return results
  end

  local function query_workspace_symbols(pattern)
    local results_by_client, err = lsp.buf_request_sync(0, 'workspace/symbol', { query = pattern }, 1000)
    assert(not err, vim.inspect(err))
    local results = {}
    for _, symbols in pairs(results_by_client) do
      for _, symbol in pairs(symbols.result or {}) do
        local loc = symbol.location
        local item = mk_tag_item(symbol.name, loc.range, loc.uri)
        item.kind = (lsp.protocol.SymbolKind[symbol.kind] or 'Unknown')[1]
        table.insert(results, item)
      end
    end
    return results
  end

  function M.tagfunc(pattern, flags)
    if flags == 'c' then
      return query_definition(pattern)
    elseif flags == '' or flags == 'i' then
      return query_workspace_symbols(pattern)
    else
      return vim.NIL
    end
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
end
end


return M
