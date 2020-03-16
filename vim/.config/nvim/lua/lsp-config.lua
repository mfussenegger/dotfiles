local myutil = require 'util'
local lsp = require 'vim.lsp'
local lsp_ext = require 'lsp-ext'
local lsp_diag = require 'lsp-diagnostics'
local api = vim.api

local lsps_dirs = {}



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
    api.nvim_buf_attach(bufnr, false, {
        on_changedtick=function(_, changedtick)
            lsp_ext.ticks[bufnr] = changedtick
        end;
        on_detach=function(_)
            lsp_ext.ticks[bufnr] = nil
        end;
    })
end


-- array of mappings to setup; {<capability_name>, <mode>, <lhs>, <rhs>}
local key_mappings = {
    {"code_action", "n", "<a-CR>", "<Cmd>lua require'lsp-ext'.code_action()<CR>"},
    {"document_formatting", "n", "gq", "<Cmd>lua vim.lsp.buf.formatting()<CR>"},
    {"document_range_formatting", "v", "gq", "<Cmd>lua vim.lsp.buf.range_formatting()<CR>"},
    {"find_references", "n", "gr", "<Cmd>lua vim.lsp.buf.references()<CR>"},
    {"goto_definition", "n", "<c-]>",  "<Cmd>lua vim.lsp.buf.definition()<CR>"},
    {"hover", "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>"},
    {"implementation", "n", "gD",  "<Cmd>lua vim.lsp.buf.implementation()<CR>"},
    {"signature_help", "i", "<c-space>",  "<Cmd>lua vim.lsp.buf.signature_help()<CR>"}
}

local function on_init(client, _)
    api.nvim_command("augroup LspExt")
    api.nvim_command("autocmd InsertCharPre * lua require'lsp-ext'._InsertCharPre()")
    api.nvim_command("autocmd InsertLeave * lua require'lsp-ext'._InsertLeave()")
    api.nvim_command("autocmd BufEnter,CursorMoved,CursorMovedI * lua require'lsp-diagnostics'.show_diagnostics()")
    api.nvim_command("autocmd CompleteDone * lua require'lsp-ext'._CompleteDone()")
    api.nvim_command("augroup end")
    lsp_ext.setup(client)
end

local function on_attach(client, bufnr)
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
    api.nvim_buf_set_keymap(bufnr, "n", "]w", "<Cmd>lua require'lsp-diagnostics'.next_diag()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "[w", "<Cmd>lua require'lsp-diagnostics'.prev_diag()<CR>", opts)
end

local function mk_config()
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities.textDocument.completion.completionItem.snippetSupport = true
    return {
        callbacks = {
            ["textDocument/publishDiagnostics"] = lsp_diag.publishDiagnostics,
            ['textDocument/declaration'] = lsp_ext.location_callback(true),
            ['textDocument/definition'] = lsp_ext.location_callback(true),
            ['textDocument/typeDefinition'] = lsp_ext.location_callback(true),
            ['textDocument/implementation'] = lsp_ext.location_callback(true),
            ['textDocument/references'] = lsp_ext.location_callback(false),
        };
        capabilities = capabilities;
        on_init = on_init;
        on_attach = on_attach;
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
    config['init_options'] = {
        settings = {
            java = {
                signatureHelp = { enabled = true };
                completion = {
                    favoriteStaticMembers = {
                        "org.hamcrest.MatcherAssert.assertThat",
                        "org.hamcrest.Matchers.*",
                        "org.hamcrest.CoreMatchers.*",
                    }
                }
            };
        };
        extendedClientCapabilities = {
            classFileContentsSupport = true
        };
    }
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
