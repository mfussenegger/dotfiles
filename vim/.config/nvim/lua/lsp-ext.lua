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
        table.sort(items, function(a, b) return (a.sortText or 0) < (b.sortText or 0) end)
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
    local text_match = vim.fn.match(line_to_cursor, '\\k*$')
    col = text_match + 1
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


function M._CompleteDone()
    local completed_item = api.nvim_get_vvar('completed_item')
    if not completed_item or not completed_item.user_data then
      return
    end
    completion_ctx.col = nil
    local lnum, col = unpack(api.nvim_win_get_cursor(0))
    local item = completed_item.user_data
    local bufnr = api.nvim_get_current_buf()
    if item.additionalTextEdits then
      -- Text edit in the same line would mess with the cursor position
      local edits = vim.tbl_filter(
        function(x) return x.range.start.line ~= (lnum - 1) end,
        item.additionalTextEdits
      )
      vim.lsp.util.apply_text_edits(edits, bufnr)
    end
    -- 2 is snippet
    if item.insertTextFormat ~= 2 or not completion_ctx.expand_snippet then
      return
    end
    -- Create textEdit to remove the already inserted word
    local text_edit = {
      range = {
        ["start"] = {
          line = lnum - 1;
          character = (col - #completed_item.word);
        };
        ["end"] = {
          line = lnum - 1;
          character = col;
        }
      };
      newText = "";
    }
    vim.lsp.util.apply_text_edits({text_edit}, bufnr)
    completion_ctx.expand_snippet = false
    if item.textEdit then
      api.nvim_call_function("UltiSnips#Anon", {item.textEdit.newText})
    else
      api.nvim_call_function("UltiSnips#Anon", {item.insertText})
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
end

return M
