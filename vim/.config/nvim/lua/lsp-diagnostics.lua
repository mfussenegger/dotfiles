local myutil = require 'util'
local api = vim.api
local protocol = vim.lsp.protocol

-- {<bufnr>: {<lineNr>: {diagnostics}}}
local sign_ns = 'vim_lsp_signs'
local timer = nil
local M = {}

local updatedDiagnostics = false
local nextDiag = 0

M.diagnostics_by_buffer = {}


local function diagnostics_to_items(bufnr, buf_diagnostics)
    local items = {}
    if not buf_diagnostics then return items end
    for linenr, diagnostics in pairs(buf_diagnostics) do
        if #diagnostics > 0 then
            local d = diagnostics[1]
            table.insert(items, {
                bufnr = bufnr,
                lnum = linenr + 1,
                vcol = 1,
                col = d.range.start.character + 1,
                text = d.message
            })
        end
    end
    return items
end


local function set_signs(bufnr, buf_diagnostics)
    vim.fn.sign_unplace(sign_ns, {buffer=bufnr})
    if not buf_diagnostics then return end
    vim.fn.sign_define('LspDiagnosticsErrorSign', {text='E', texthl='LspDiagnosticsError', linehl='', numhl=''})
    vim.fn.sign_define('LspDiagnosticsWarningSign', {text='W', texthl='LspDiagnosticsWarning', linehl='', numhl=''})
    vim.fn.sign_define('LspDiagnosticsInformationSign', {text='I', texthl='LspDiagnosticsInformation', linehl='', numhl=''})
    vim.fn.sign_define('LspDiagnosticsHintSign', {text='H', texthl='LspDiagnosticsHint', linehl='', numhl=''})
    local severity_map = {
        [protocol.DiagnosticSeverity.Error] = "LspDiagnosticsErrorSign";
        [protocol.DiagnosticSeverity.Warning] = "LspDiagnosticsWarningSign";
        [protocol.DiagnosticSeverity.Information] = "LspDiagnosticsInformationSign";
        [protocol.DiagnosticSeverity.Hint] = "LspDiagnosticsHintSign";
    }
    for _, diagnostics in pairs(buf_diagnostics) do
        if #diagnostics > 0 then
            local d = diagnostics[1]
            vim.fn.sign_place(
                0,
                sign_ns,
                severity_map[d.severity],
                bufnr,
                { lnum = (d.range.start.line + 1) }
            )
        end
    end
end


local function save_diagnostics(bufnr, diagnostics, uri)
    if not diagnostics then return end
    local buf_diagnostics = {}
    M.diagnostics_by_buffer[bufnr] = buf_diagnostics
    for _, diagnostic in ipairs(diagnostics) do
        local start = diagnostic.range.start
        local line_diagnostics = buf_diagnostics[start.line]
        if not line_diagnostics then
            line_diagnostics = {}
            buf_diagnostics[start.line] = line_diagnostics
        end
        diagnostic.uri = uri
        table.insert(line_diagnostics, diagnostic)
    end
end


function M.publishDiagnostics(_, _, result)
    if not result then return end
    local uri = result.uri
    local bufnr = vim.uri_to_bufnr(uri)
    if not bufnr then
        myutil.err_message("LSP.publishDiagnostics: Couldn't find buffer for ", uri)
        return
    end
    save_diagnostics(bufnr, result.diagnostics, result.uri)
    updatedDiagnostics = true
    M.show_diagnostics()
end


function M.show_diagnostics()
    if timer then
        timer:stop()
        timer:close()
        timer = nil
    end
    timer = vim.loop.new_timer()
    timer:start(250, 0, vim.schedule_wrap(function()
        timer:close()
        timer = nil

        local bufnr = api.nvim_get_current_buf()
        local buf_diagnostics = M.diagnostics_by_buffer[bufnr]
        if buf_diagnostics and api.nvim_get_mode()['mode'] == 'n' then
            local row = api.nvim_win_get_cursor(0)[1]
            local d = buf_diagnostics[row - 1]
            if d and #d > 0 and d[1].message then
                print(d[1].message)
            end
        end

        if not updatedDiagnostics then
            return
        end
        updatedDiagnostics = false
        nextDiag = 0

        vim.fn.setloclist(0, {}, ' ', {
            title = 'Language Server';
            items = diagnostics_to_items(bufnr, buf_diagnostics)
        })
        set_signs(bufnr, buf_diagnostics)
    end))
end


local function diagnostics_sorted_by_severity()
    local all_diagnostics = {}
    for _, buf_diagnostics in pairs(M.diagnostics_by_buffer) do
        for _, line_diagnostics in pairs(buf_diagnostics) do
            local last_col = nil
            for _, d in pairs(line_diagnostics) do
                local fname = vim.uri_to_fname(d.uri)
                local stat = vim.loop.fs_stat(fname)
                local is_dir = stat and stat.type == 'directory'
                if not is_dir and (last_col == nil or last_col ~= d.range.start.character) then
                    last_col = d.range.start.character
                    table.insert(all_diagnostics, d)
                end
            end
        end
    end
    table.sort(all_diagnostics, function(a, b)
        if a.severity == b.severity then
            return a.range.start.line < b.range.start.line
        else
            return a.severity < b.severity
        end
    end)
    return all_diagnostics
end


function M.next_diag()
    local diagnostics = diagnostics_sorted_by_severity()
    if #diagnostics == 0 then
        return
    end
    nextDiag = nextDiag + 1
    if nextDiag > #diagnostics then
        nextDiag = 1
    end
    local d = diagnostics[nextDiag]
    local bufnr = vim.uri_to_bufnr(d.uri)
    myutil.jump_to_buf(bufnr, d.range)
end


function M.prev_diag()
    local diagnostics = diagnostics_sorted_by_severity()
    if #diagnostics == 0 then
        return
    end
    nextDiag = nextDiag - 1
    if nextDiag < 1 then
        nextDiag = #diagnostics
    end
    local d = diagnostics[nextDiag]
    local bufnr = vim.uri_to_bufnr(d.uri)
    myutil.jump_to_buf(bufnr, d.range)
end


return M
