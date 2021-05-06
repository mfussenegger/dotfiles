local api = vim.api
local lsp = vim.lsp
local timer = nil
local triggers_by_buf = {}
local M = {}
local snippet = 2

local request = function(method, payload, handler)
  return lsp.buf_request(0, method, payload, handler)
end

local completion_ctx
completion_ctx = {
  expand_snippet = false,
  col = nil,

  pending_requests = {},
  cancel_pending = function()
    for _, cancel in pairs(completion_ctx.pending_requests) do
      cancel()
    end
    completion_ctx.pending_requests = {}
  end,
  reset = function()
    completion_ctx.expand_snippet = false
    completion_ctx.col = nil
    completion_ctx.cancel_pending()
  end
}


local function get_documentation(item)
  local docs = item.documentation
  if type(docs) == 'string' then
    return docs
  end
  if type(docs) == 'table' and type(docs.value) == 'string' then
    return docs.value
  end
  return ''
end


local function text_document_completion_list_to_complete_items(result, prefix, fuzzy)
  local items = lsp.util.extract_completion_items(result)
  if #items == 0 then
    return {}
  end
  if items[1] and items[1].sortText then
    table.sort(items, function(a, b) return (a.sortText or a.label) < (b.sortText or b.label) end)
  end
  local matches = {}
  for _, item in pairs(items) do
    local info = get_documentation(item)
    local kind = lsp.protocol.CompletionItemKind[item.kind] or ''
    local word
    if kind == 'Snippet' then
      word = item.label
    elseif item.insertTextFormat == snippet then
      --[[
      -- eclipse.jdt.ls has
      --      insertText = "wait",
      --      label = "wait() : void"
      --      textEdit = { ... }
      --
      -- haskell-ide-engine has
      --      insertText = "testSuites ${1:Env}"
      --      label = "testSuites"
      --]]
      word = item.textEdit and (item.insertText or item.textEdit.newText) or item.label
    else
      word = (item.textEdit and item.textEdit.newText) or item.insertText or item.label
    end
    if fuzzy or vim.startswith(word, prefix) then
      table.insert(matches, {
        word = word,
        abbr = item.label,
        kind = kind,
        menu = item.detail or '',
        info = info,
        icase = 1,
        dup = 1,
        empty = 1,
        equal = fuzzy and 1 or 0,
        user_data = item
      })
    end
  end
  return matches
end


local function find_start(line, cursor_pos)
  local line_to_cursor = line:sub(1, cursor_pos)
  local idx = 0
  while true do
    local i = string.find(line_to_cursor, '[^a-zA-Z0-9_]', idx + 1)
    if i == nil then
      break
    else
      idx = i
    end
  end
  return idx + 1
end


local function reset_timer()
  if timer then
    timer:stop()
    timer:close()
    timer = nil
  end
end


function M.trigger_completion()
  reset_timer()
  completion_ctx.cancel_pending()
  local cursor_pos = api.nvim_win_get_cursor(0)[2]
  local line = api.nvim_get_current_line()
  local col = completion_ctx.col or find_start(line, cursor_pos)
  local prefix = line:sub(col, cursor_pos)
  local params = lsp.util.make_position_params()
  local _, cancel_req = request('textDocument/completion', params, function(err, _, result, client_id)
    completion_ctx.pending_requests = {}
    assert(not err, vim.inspect(err))
    if not result then
      print('No completion result')
      return
    end
    local mode = api.nvim_get_mode()['mode']
    if mode == 'i' or mode == 'ic' then
      local client = vim.lsp.get_client_by_id(client_id)
      local matches = text_document_completion_list_to_complete_items(
        result,
        prefix,
        client and client.config.flags.server_side_fuzzy_completion
      )
      vim.fn.complete(col, matches)
    end
  end)
  table.insert(completion_ctx.pending_requests, cancel_req)
end


function M._InsertCharPre(server_side_fuzzy_completion)
  if timer then
    return
  end
  if tonumber(vim.fn.pumvisible()) == 1 then
    if server_side_fuzzy_completion then
      timer = vim.loop.new_timer()
      timer:start(150, 0, vim.schedule_wrap(M.trigger_completion))
    end
    return
  end
  local char = api.nvim_get_vvar('char')
  local triggers = triggers_by_buf[api.nvim_get_current_buf()] or {}
  for _, entry in pairs(triggers) do
    local chars, fn = unpack(entry)
    if vim.tbl_contains(chars, char) then
      completion_ctx.col = nil
      timer = vim.loop.new_timer()
      timer:start(50, 0, function()
        reset_timer()
        vim.schedule(fn)
      end)
      return
    end
  end
end


function M._InsertLeave()
  reset_timer()
  completion_ctx.reset()
end


local function apply_text_edits(bufnr, lnum, text_edits)
  -- Text edit in the same line would mess with the cursor position
  local edits = vim.tbl_filter(
    function(x) return x.range.start.line ~= lnum end,
    text_edits or {}
  )
  lsp.util.apply_text_edits(edits, bufnr)
end


local function apply_snippet(item, suffix)
  if item.textEdit then
    vim.fn['vsnip#anonymous'](item.textEdit.newText .. suffix)
  elseif item.insertText then
    vim.fn['vsnip#anonymous'](item.insertText .. suffix)
  end
end


function M._CompleteDone(resolveEdits)
  local completed_item = api.nvim_get_vvar('completed_item')
  if not completed_item or not completed_item.user_data then
    completion_ctx.reset()
    return
  end
  local lnum, col = unpack(api.nvim_win_get_cursor(0))
  lnum = lnum - 1
  local item = completed_item.user_data
  local bufnr = api.nvim_get_current_buf()
  local expand_snippet = item.insertTextFormat == snippet and completion_ctx.expand_snippet
  local suffix = nil
  if expand_snippet then
    -- Remove the already inserted word
    local start_char = completion_ctx.col and (completion_ctx.col - 1) or (col - #completed_item.word)
    local line = api.nvim_buf_get_lines(bufnr, lnum, lnum + 1, true)[1]
    suffix = line:sub(col + 1)
    api.nvim_buf_set_text(bufnr, lnum, start_char, lnum, #line, {''})
  end
  completion_ctx.reset()
  if item.additionalTextEdits then
    apply_text_edits(bufnr, lnum, item.additionalTextEdits)
    if expand_snippet then
      apply_snippet(item, suffix)
    end
  elseif resolveEdits then
    local _, cancel_req = request('completionItem/resolve', item, function(err, _, result)
      completion_ctx.pending_requests = {}
      assert(not err, vim.inspect(err))
      apply_text_edits(bufnr, lnum, result.additionalTextEdits)
      if expand_snippet then
        apply_snippet(item, suffix)
      end
    end)
    table.insert(completion_ctx.pending_requests, cancel_req)
  elseif expand_snippet then
    apply_snippet(item, suffix)
  end
end



function M.accept_pum()
  if tonumber(vim.fn.pumvisible()) == 0 then
    return false
  else
    completion_ctx.expand_snippet = true
    return true
  end
end


function M.setup()
  lsp.util.text_document_completion_list_to_complete_items = text_document_completion_list_to_complete_items
end


function M.attach(client, bufnr)
  vim.cmd(string.format('augroup lsp_ext_%d_%d', client.id, bufnr))
  vim.cmd('au!')
  vim.cmd(string.format(
    "autocmd InsertCharPre <buffer=%d> lua require'me.lsp.ext'._InsertCharPre(%s)",
    bufnr,
    client.config.flags.server_side_fuzzy_completion
  ))
  vim.cmd(string.format("autocmd InsertLeave <buffer=%d> lua require'me.lsp.ext'._InsertLeave()", bufnr))
  vim.cmd(string.format(
    "autocmd CompleteDone <buffer=%d> lua require'me.lsp.ext'._CompleteDone(%s)",
    bufnr,
    (client.server_capabilities.completionProvider or {}).resolveProvider
  ))
  vim.cmd('augroup end')

  local triggers = triggers_by_buf[bufnr]
  if not triggers then
    triggers = {}
    triggers_by_buf[bufnr] = triggers
    api.nvim_buf_attach(bufnr, false, {
      on_detach = function(_, b)
        triggers_by_buf[b] = nil
      end
    })
  end
  local signature_triggers = client.resolved_capabilities.signature_help_trigger_characters
  if signature_triggers and #signature_triggers > 0 then
    table.insert(triggers, { signature_triggers, lsp.buf.signature_help })
  end
  local completionProvider = client.server_capabilities.completionProvider or {}
  local completion_triggers = completionProvider.triggerCharacters
  if completion_triggers and #completion_triggers > 0 then
    table.insert(triggers, { completion_triggers, M.trigger_completion })
  end
end



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
        item.kind = lsp.protocol.SymbolKind[symbol.kind] or 'Unknown'
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
end


return M
