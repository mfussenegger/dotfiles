local api = vim.api
local timer = nil
local on_insert_with_pause = {}

local completion_ctx = {
  expand_snippet = false,
  col = nil
}


local M = {}


local function text_document_completion_list_to_complete_items(result, _)
    local items = vim.lsp.util.extract_completion_items(result)
    if #items == 0 then
        return {}
    end
    if items[1] and items[1].sortText then
        table.sort(items, function(a, b) return (a.sortText or '0') < (b.sortText or '0') end)
    end

    local matches = {}
    for _, item in ipairs(items) do
        local info = ''
        local documentation = item.documentation
        if documentation then
            if type(documentation) == 'string' and documentation ~= '' then
                info = documentation
            elseif type(documentation) == 'table' and type(documentation.value) == 'string' then
                info = documentation.value
            end
        end
        local kind = vim.lsp.protocol.CompletionItemKind[item.kind] or ''
        local word
        if kind == 'Snippet' then
            word = item.label
        elseif item.insertTextFormat == 2 then -- 2 == snippet
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
            if item.textEdit then
                word = item.insertText
            else
                word = item.label
            end
        else
            word = (item.textEdit and item.textEdit.newText) or item.insertText or item.label
        end
        table.insert(matches, {
            word = word,
            abbr = item.label,
            kind = kind,
            menu = item.detail or '',
            info = info,
            icase = 1,
            dup = 1,
            empty = 1,
            equal = 1,
            user_data = item
        })
    end
    return matches
end


function M.trigger_completion()
  local bufnr = api.nvim_get_current_buf()
  local _, col = unpack(api.nvim_win_get_cursor(0))
  if completion_ctx.col then
    col = completion_ctx.col
  else
    local line = api.nvim_get_current_line()
    local line_to_cursor = line:sub(1, col)
    local idx = 0
    while true do
      local i = string.find(line_to_cursor, '[^a-zA-Z0-9_]', idx + 1)
      if i == nil then
        break
      else
        idx = i
      end
    end
    col = (idx or col) + 1
    completion_ctx.col = col
  end
  local params = vim.lsp.util.make_position_params()
  vim.lsp.buf_request(bufnr, 'textDocument/completion', params, function(err, _, result)
    if err then
      print('Error getting completions: ' .. err.message)
      return
    end
    if not result then
      print('no completion result')
      return
    end
    local mode = api.nvim_get_mode()['mode']
    if mode == 'i' or mode == 'ic' then
      local matches = text_document_completion_list_to_complete_items(result)
      vim.fn.complete(col, matches)
    end
  end)
end


function M._InsertCharPre()
    if timer then
        timer:stop()
        timer:close()
        timer = nil
    end
    local char = api.nvim_get_vvar('char')
    timer = vim.loop.new_timer()
    for _, entry in pairs(on_insert_with_pause) do
        local chars, fn = unpack(entry)
        if vim.tbl_contains(chars, char) then
            completion_ctx.col = nil
            timer:start(150, 0, vim.schedule_wrap(function() fn() end))
            return
        end
    end
    if tonumber(vim.fn.pumvisible()) == 1 then
      timer:start(150, 0, vim.schedule_wrap(function() M.trigger_completion() end))
    end
end


function M._InsertLeave()
    if timer then
        timer:stop()
        timer:close()
        timer = nil
    end
    completion_ctx.col = nil
end


function M._CompleteChanged()
  -- No-op by default. Activated in setup if client supports completionItem/resolve
end

local function complete_changed()
  completion_ctx.additionalTextEdits = nil
  local completed_item = api.nvim_get_vvar('completed_item')
  if not completed_item or not completed_item.user_data then
    return
  end
  vim.lsp.buf_request(0, 'completionItem/resolve', completed_item.user_data, function(err, _, result)
    if err then
      print('Error on completionItem/resolve: ', err.message)
      return
    end
    completion_ctx.additionalTextEdits = result and result.additionalTextEdits
  end)
end

function M._CompleteDone()
    local completion_start_idx = completion_ctx.col
    local resolved_additionalTextEdits = completion_ctx.additionalTextEdits
    completion_ctx.additionalTextEdits = nil
    completion_ctx.col = nil
    local completed_item = api.nvim_get_vvar('completed_item')
    if not completed_item or not completed_item.user_data then
      return
    end
    local lnum, col = unpack(api.nvim_win_get_cursor(0))
    lnum = lnum - 1
    local item = completed_item.user_data
    if type(item) == 'string' then
      return
    end
    local bufnr = api.nvim_get_current_buf()
    local expand_snippet = item.insertTextFormat == 2 and completion_ctx.expand_snippet
    completion_ctx.expand_snippet = false
    local suffix = nil

    if expand_snippet then
      -- Create textEdit to remove the already inserted word
      local start_char = completion_start_idx and (completion_start_idx - 1) or (col - #completed_item.word)
      local line = api.nvim_buf_get_lines(bufnr, lnum, lnum + 1, true)[1]
      suffix = line:sub(col + 1)
      local text_edit = {
        range = {
          ["start"] = {
            line = lnum;
            character = start_char;
          };
          ["end"] = {
            line = lnum;
            character = #line;
          }
        };
        newText = "";
      }
      vim.lsp.util.apply_text_edits({text_edit}, bufnr)
    end

    if not item.additionalTextEdits then
      item.additionalTextEdits = resolved_additionalTextEdits
    end
    if item.additionalTextEdits then
      -- Text edit in the same line would mess with the cursor position
      local edits = vim.tbl_filter(
        function(x) return x.range.start.line ~= lnum end,
        item.additionalTextEdits
      )
      local ok, err = pcall(vim.lsp.util.apply_text_edits, edits, bufnr)
      if not ok then
        print(err, vim.inspect(edits))
      end
    end
    if expand_snippet then
      if item.textEdit then
        api.nvim_call_function("UltiSnips#Anon", {item.textEdit.newText .. suffix})
      else
        api.nvim_call_function("UltiSnips#Anon", {item.insertText .. suffix})
      end
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

function M.setup(client)
    local signature_triggers = client.resolved_capabilities.signature_help_trigger_characters
    if signature_triggers and #signature_triggers > 0 then
        table.insert(
            on_insert_with_pause, { signature_triggers, vim.lsp.buf.signature_help }
        )
    end
    local completionProvider = client.server_capabilities.completionProvider or {}
    local completion_triggers = completionProvider.triggerCharacters
    if completion_triggers and #completion_triggers > 0 then
        table.insert(
            on_insert_with_pause, { completion_triggers, M.trigger_completion }
        )
    end
    vim.lsp.util.text_document_completion_list_to_complete_items = text_document_completion_list_to_complete_items
    if (client.server_capabilities.completionProvider or {}).resolveProvider then
      M._CompleteChanged = complete_changed
    end
end

return M
