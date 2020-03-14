local uv = vim.loop
local api = vim.api
local log = vim.lsp.log

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


local function jump_to_buf(buf, range)
    api.nvim_set_current_buf(buf)
    local row = range.start.line
    local col = range.start.character
    local line = api.nvim_buf_get_lines(0, row, row + 1, true)[1]
    col = vim.str_byteindex(line, col)
    api.nvim_win_set_cursor(0, { row + 1, col })
end


local function openJdtLink(uri, range)
    local bufnr = api.nvim_get_current_buf()
    local params = {
        uri = uri
    }
    vim.lsp.buf_request(bufnr, 'java/classFileContents', params, function(err, _, content)
        if err then return end
        local buf = api.nvim_create_buf(false, true)
        api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(content, '\n', true))
        api.nvim_buf_set_option(buf, 'filetype', 'java')
        jump_to_buf(buf, range)
    end)
end


local function isJdtLinkLocation(location)
    return location.uri and location.uri:sub(1, 6) == "jdt://"
end


function M.location_callback(autojump)
    return function(_, method, result)
        if result == nil or #result == 0 then
            local _ = log.info() and log.info(method, 'No location found')
            return nil
        end
        if not autojump or #result > 1 then
            vim.fn.setqflist({}, ' ', {
                title = 'Language Server';
                items = vim.lsp.util.locations_to_items(
                    vim.tbl_filter(
                        function(loc) return not isJdtLinkLocation(loc) end,
                        result
                    )
                )
            })
            api.nvim_command("copen")
            api.nvim_command("wincmd p")
        elseif result[1].uri ~= nil then
            vim.cmd "normal! m'" -- save position in jumplist
            local location = result[1]
            if location.uri:sub(1, 6) == "jdt://" then
                openJdtLink(location.uri, location.range)
            else
                jump_to_buf(vim.uri_to_bufnr(location.uri), location.range)
            end
        end
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
            on_insert_with_pause, { completion_triggers, trigger_completion }
        )
    end
    api.nvim_command("autocmd InsertCharPre * lua require'lsp-ext'._InsertCharPre()")
    api.nvim_command("autocmd InsertLeave * lua require'lsp-ext'._InsertLeave()")
end

return M
