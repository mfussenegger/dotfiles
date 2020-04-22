local api = vim.api
local timer = nil
local on_insert_with_pause = {}
local expand_snippet = false

local M = {}


function M._InsertCharPre()
    if timer then
        timer:stop()
        timer:close()
    end
    timer = vim.loop.new_timer()
    local char = api.nvim_get_vvar('char')
    for _, entry in pairs(on_insert_with_pause) do
        local chars, fn = unpack(entry)
        if vim.tbl_contains(chars, char) then
            timer:start(150, 0, vim.schedule_wrap(function()
                fn()
            end))
            return
        end
    end
end


function M._InsertLeave()
    if timer then
        timer:stop()
        timer:close()
        timer = nil
    end
end


local function trigger_completion()
    local bufnr = api.nvim_get_current_buf()
    local pos = api.nvim_win_get_cursor(0)
    local line = api.nvim_get_current_line()
    local line_to_cursor = line:sub(1, pos[2])
    local textMatch = vim.fn.match(line_to_cursor, '\\k*$')
    local prefix = line_to_cursor:sub(textMatch+1)
    local params = vim.lsp.util.make_position_params()
    vim.lsp.buf_request(bufnr, 'textDocument/completion', params, function(err, _, result)
        if err or not result then return end
        if api.nvim_get_mode()['mode'] == 'i' then
            local matches = vim.lsp.util.text_document_completion_list_to_complete_items(result, prefix)
            vim.fn.complete(textMatch + 1, matches)
        end
  end)
end


function M._CompleteDone()
    local completed_item = api.nvim_get_vvar('completed_item')
    if not completed_item or not completed_item.user_data or completed_item.user_data == '' then
        return
    end
    local item = vim.fn.json_decode(completed_item.user_data)

    -- 2 is snippet
    if item.insertTextFormat ~= 2 or not expand_snippet then
        return
    end
    expand_snippet = false
    local row, pos = unpack(api.nvim_win_get_cursor(0))
    -- Create textEdit to remove the already inserted word
    local text_edit = {
        range = {
            ["start"] = {
                line = row - 1;
                character = (pos - #completed_item.word);
            };
            ["end"] = {
                line = row - 1;
                character = pos;
            }
        };
        newText = "";
    }
    vim.lsp.util.apply_text_edits({text_edit}, api.nvim_get_current_buf())

    if item.textEdit then
        api.nvim_call_function("UltiSnips#Anon", {item.textEdit.newText})
    else
        api.nvim_call_function("UltiSnips#Anon", {item.insertText})
    end
end


local function text_document_completion_list_to_complete_items(result, prefix)
    local items = vim.tbl_filter(function(item)
        return (item.insertText and vim.startswith(item.insertText, prefix))
            or (item.label and vim.startswith(item.label, prefix))
            or (item.textEdit and item.textEdit.newText and vim.startswith(item.textEdit.newText, prefix))
    end, vim.lsp.util.extract_completion_items(result))
    if #items == 0 then
        return {}
    end
    if items[1] and items[1].sortText then
        table.sort(items, function(a, b) return (a.sortText or 0) < (b.sortText or 0) end)
    end

    local matches = {}
    for _, item in ipairs(items) do
        local info = ' '
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
            user_data = vim.fn.json_encode(item)
        })
    end
    return matches
end


function M.accept_pum()
    if tonumber(vim.fn.pumvisible()) == 0 then
        return false
    else
        expand_snippet = true
        return true
    end
end


function M.workspace_symbol()
  local query = vim.fn.input("Symbol: ")
  local params = {
    query = query
  }
  local bufnr = api.nvim_get_current_buf()
  vim.lsp.buf_request(bufnr, 'workspace/symbol', params, function(err, _, result)
    if err then return end
    if not result then
      print("No symbols matching " .. query .. " found")
    end
    local items = {}
    for _, s in ipairs(result) do
      table.insert(items, {
        filename = vim.uri_to_fname(s.location.uri),
        lnum = s.location.range.start.line + 1,
        vcol = 1,
        col = s.location.range.start.character + 1,
        text = s.name
      })
    end
    vim.fn.setqflist({}, ' ', {
      title = 'Workspace Symbols';
      items = items;
    })
    api.nvim_command("copen")
    api.nvim_command("wincmd p")
  end)
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
            on_insert_with_pause, { completion_triggers, trigger_completion }
        )
    end
    vim.lsp.util.text_document_completion_list_to_complete_items = text_document_completion_list_to_complete_items
end

return M
