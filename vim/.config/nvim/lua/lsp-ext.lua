local uv = vim.loop
local api = vim.api

local timer = uv.new_timer()
local on_insert_with_pause = {}

local M = {}


function M._InsertCharPre()
    local char = api.nvim_get_vvar('char')
    local func = nil
    for _, entry in pairs(on_insert_with_pause) do
        local chars, fn = unpack(entry)
        if vim.tbl_contains(chars, char) then
            func = fn
        else
            timer:stop()
        end
    end
    if func then
        timer:start(150, 0, vim.schedule_wrap(function()
            timer:stop()
            func()
        end))
    else
        timer:stop()
    end
end


function M._InsertLeave()
    timer:stop()
end


local function trigger_completion()
    local bufnr = api.nvim_get_current_buf()
    local pos = api.nvim_win_get_cursor(0)
    local line = api.nvim_get_current_line()
    local line_to_cursor = line:sub(1, pos[2])
    local textMatch = vim.fn.match(line_to_cursor, '\\k*$')
    local prefix = line_to_cursor:sub(textMatch+1)
    local params = vim.lsp.util.make_position_params()
    local items = {}
    vim.lsp.buf_request(bufnr, 'textDocument/completion', params, function(err, _, result)
        if err or not result then return end
        local matches = vim.lsp.util.text_document_completion_list_to_complete_items(result, prefix)
        vim.list_extend(items, matches)
        vim.fn.complete(textMatch + 1, items)
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
    api.nvim_command("autocmd InsertCharPre * lua require'lsp-ext'._InsertCharPre()")
    api.nvim_command("autocmd InsertLeave * lua require'lsp-ext'._InsertLeave()")
end

return M
