local uv = vim.loop
local api = vim.api

local timer = uv.new_timer()
local on_insert_with_pause = {}

local M = {}


function M._InsertCharPre()
    local char = api.nvim_get_vvar('char')
    timer:start(100, 0, vim.schedule_wrap(function()
        for _, entry in pairs(on_insert_with_pause) do
            local chars, func = unpack(entry)
            if vim.tbl_contains(chars, char) then
                func()
            end
        end
    end))
end


function M._InsertLeave()
    timer:stop()
end


function M.setup(client)
    local signature_triggers = client.resolved_capabilities.signature_help_trigger_characters
    if signature_triggers and #signature_triggers > 0 then
        table.insert(
            on_insert_with_pause, { signature_triggers, vim.lsp.buf.signature_help }
        )
    end
    api.nvim_command("autocmd InsertCharPre * lua require'lsp-ext'._InsertCharPre()")
    api.nvim_command("autocmd InsertLeave * lua require'lsp-ext'._InsertLeave()")
end

return M
