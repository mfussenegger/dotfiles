local myutil = require 'util'
local api = vim.api
local protocol = vim.lsp.protocol

-- {<bufnr>: {<lineNr>: {diagnostics}}}
local sign_ns = 'vim_lsp_signs'
local timer = vim.loop.new_timer()
local M = {}

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


local function save_diagnostics(bufnr, diagnostics)
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
    save_diagnostics(bufnr, result.diagnostics)
    M.show_diagnostics()
end


function M.show_diagnostics()
    timer:start(250, 0, vim.schedule_wrap(function()
        timer:stop()
        local bufnr = api.nvim_get_current_buf()
        local buf_diagnostics = M.diagnostics_by_buffer[bufnr]
        vim.fn.setloclist(0, {}, ' ', {
            title = 'Language Server';
            items = diagnostics_to_items(bufnr, buf_diagnostics)
        })
        set_signs(bufnr, buf_diagnostics)
    end))
end


return M
