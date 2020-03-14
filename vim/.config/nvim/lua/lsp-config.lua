local myutil = require 'util'
local lsp = require 'vim.lsp'
local api = vim.api

local lsps_dirs = {}


local function diagnostics_to_items(bufnr, diagnostics)
    local items = {}
    if not diagnostics then return items end
    for _, diagnostic in pairs(diagnostics) do
        table.insert(items, {
            bufnr = bufnr,
            lnum = diagnostic.range.start.line + 1,
            vcol = 1,
            col = diagnostic.range.start.character,
            text = diagnostic.message
        })
    end
    return items
end

local default_diagnostics_callback = lsp.callbacks['textDocument/publishDiagnostics']
local function diagnostics_callback(err, method, result, client_id)
    default_diagnostics_callback(err, method, result, client_id)
    if result and result.diagnostics and result.uri then
        local current_buf = api.nvim_get_current_buf()
        if vim.uri_to_bufnr(result.uri) == current_buf and myutil.exists(vim.uri_to_fname(result.uri)) then
            for _, v in ipairs(result.diagnostics) do
                v.uri = v.uri or result.uri
            end
            vim.fn.setloclist(0, {}, ' ', {
                title = 'Language Server';
                items = diagnostics_to_items(current_buf, result.diagnostics);
            })
        end
    end
end

local function add_client_by_cfg(config, root_markers)
    local bufnr = api.nvim_get_current_buf()
    local root_dir = myutil.root_pattern(bufnr, root_markers)
    if not root_dir then return end

    local cmd = config.cmd[1]
    if tonumber(vim.fn.executable(cmd)) == 0 then
        api.nvim_command(string.format(
            ':echohl WarningMsg | redraw | echo "No LSP executable: %s" | echohl None', cmd))
        return
    end
    config['root_dir'] = root_dir
    local client_id = lsps_dirs[root_dir]
    if not client_id then
        client_id = lsp.start_client(config)
        lsps_dirs[root_dir] = client_id
    end
    lsp.buf_attach_client(bufnr, client_id)
end


-- array of mappings to setup; {<capability_name>, <mode>, <lhs>, <rhs>}
local key_mappings = {
    {"code_action", "n", "<a-CR>", "<Cmd>lua vim.lsp.buf.code_action()<CR>"},
    {"document_formatting", "n", "gq", "<Cmd>lua vim.lsp.buf.formatting()<CR>"},
    {"document_range_formatting", "v", "gq", "<Cmd>lua vim.lsp.buf.range_formatting()<CR>"},
    {"find_references", "n", "gr", "<Cmd>lua vim.lsp.buf.references()<CR>"},
    {"goto_definition", "n", "<c-]>",  "<Cmd>lua vim.lsp.buf.definition()<CR>"},
    {"hover", "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>"},
    {"implementation", "n", "gD",  "<Cmd>lua vim.lsp.buf.implementation()<CR>"},
    {"signature_help", "i", "<c-space>",  "<Cmd>lua vim.lsp.buf.signature_help()<CR>"}
}

local function enable_mappings_on_buffer(client, bufnr)
    api.nvim_buf_set_var(bufnr, "lsp_client_id", client.id)
    api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
    api.nvim_buf_set_option(bufnr, "bufhidden", "hide")
    api.nvim_command("setlocal signcolumn=yes")
    api.nvim_command('ALEDisableBuffer')

    local opts = { silent = true; }
    for _, mappings in pairs(key_mappings) do
        local capability, mode, lhs, rhs = unpack(mappings)
        if client.resolved_capabilities[capability] then
            api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
        end
    end
    api.nvim_buf_set_keymap(bufnr, "n", "crr", "<Cmd>lua vim.lsp.buf.rename()<CR>", opts)
end

local function mk_config()
    return {
        callbacks = {
            ["textDocument/publishDiagnostics"] = diagnostics_callback,
        };
        on_attach = enable_mappings_on_buffer;
    }
end

local M = {}
function M.add_client(cmd, opts)
    local config = mk_config()
    config['name'] = opts and opts.name or cmd[1]
    config['cmd'] = cmd
    add_client_by_cfg(config, opts and opts.root or {'.git'})
end
function M.start_jdt()
    local lsp4j_status_callback = vim.schedule_wrap(function(_, _, result)
        api.nvim_command(string.format(':echohl Function | echo "%s" | echohl None', result.message))
    end)
    local config = mk_config()
    config['name'] = 'eclipse.jdt.ls'
    config['cmd'] = {'java-lsp.sh'}
    config['callbacks']["language/status"] = lsp4j_status_callback
    add_client_by_cfg(config, {'gradlew', '.git'})
end
function M.start_hie()
    local config = mk_config()
    config['name'] = 'hie'
    config['cmd'] = {'hie-wrapper', '--lsp'}
    config['init_options'] = {
        languageServerHaskell = {
            formattingProvider = "ormolu";
        }
    }
    add_client_by_cfg(config, {'stack.yml', '.git'})
end
function M.start_go_ls()
    local path = os.getenv("GOPATH") .. "/bin/go-langserver"
    M.add_client({path, '-gocodecompletion'}, {name = 'gols'})
end

--- @export
return M
