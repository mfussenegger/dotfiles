require 'util'

local util = require 'vim.lsp.util'
local lsp = require 'vim.lsp'
local api = vim.api

local lsps_dirs = {}

local default_diagnostics_callback = lsp.callbacks['textDocument/publishDiagnostics']
local function diagnostics_callback(err, method, result, client_id)
    default_diagnostics_callback(err, method, result, client_id)
    if result and result.diagnostics then
        local diagnostics = {}
        local current_buf = api.nvim_get_current_buf()
        for k, v in ipairs(result.diagnostics) do
            v.uri = v.uri or result.uri
            local bufnr = vim.uri_to_bufnr(v.uri)
            if bufnr == current_buf then
                diagnostics[k] = v
            end
        end
        util.set_loclist(diagnostics)
    end
end

local function add_client_by_cfg(config, root_markers)
    local bufnr = api.nvim_get_current_buf()
    local root_dir = root_pattern(bufnr, root_markers)
    if not root_dir then return end

    config['root_dir'] = root_dir
    local client_id = lsps_dirs[root_dir]
    if not client_id then
        client_id = lsp.start_client(config)
        lsps_dirs[root_dir] = client_id
    end
    lsp.buf_attach_client(bufnr, client_id)
end

local function enable_mappings_on_buffer(client, bufnr)
    api.nvim_buf_set_var(bufnr, "lsp_client_id", client.id)
    api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
    api.nvim_buf_set_option(bufnr, "bufhidden", "hide")
    api.nvim_command('ALEDisableBuffer')

    local function set_keymap(lhs, rhs)
        api.nvim_buf_set_keymap(bufnr, "n", lhs, rhs, { silent = true; })
    end
    set_keymap("gd", "<Cmd>lua vim.lsp.buf.declaration()<CR>")
    set_keymap("<c-]>", "<Cmd>lua vim.lsp.buf.definition()<CR>")
    set_keymap("1gD", "<Cmd>lua vim.lsp.buf.definition()<CR>")
    set_keymap("gD", "<Cmd>lua vim.lsp.buf.implementation()<CR>")
    set_keymap("K", "<Cmd>lua vim.lsp.buf.hover()<CR>")
    set_keymap("gr", "<Cmd>lua vim.lsp.buf.references()<CR>")
    set_keymap("<a-CR>", "<Cmd>lua vim.lsp.buf.code_action()<CR>")
    set_keymap("crr", "<Cmd>lua vim.lsp.buf.rename()<CR>")
    set_keymap("gq", "<Cmd>lua vim.lsp.buf.formatting()<CR>")
    api.nvim_buf_set_keymap(bufnr, "v", "gq", "<Cmd>lua vim.lsp.buf.range_formatting()<CR>", { silent = true; })
    api.nvim_buf_set_keymap(bufnr, "i", "<c-space>", "<Cmd>lua vim.lsp.buf.signature_help()<CR>", { silent = true; })
end

local function setup()
    local function mk_config(config)
        return {
            callbacks = {
                ["textDocument/publishDiagnostics"] = diagnostics_callback,
            };
            on_attach = enable_mappings_on_buffer;
        }
    end
    function add_client(cmd, opts)
        local config = mk_config()
        config['name'] = opts and opts.name or cmd[1]
        config['cmd'] = cmd
        add_client_by_cfg(config, opts and opts.root or {'.git'})
    end

    function start_jdt()
        local lsp4j_status_callback = vim.schedule_wrap(function(_, _, result)
            api.nvim_command(string.format(':echohl Function | echo "%s" | echohl None', result.message))
        end)
        local config = mk_config()
        config['name'] = 'eclipse.jdt.ls'
        config['cmd'] = {'java-lsp.sh'}
        config['callbacks']["language/status"] = lsp4j_status_callback
        add_client_by_cfg(config, {'gradlew', '.git'})
    end
    function start_hie()
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
    function start_go_ls()
        local path = os.getenv("GOPATH") .. "/bin/go-langserver"
        if not vim.fn.filereadable(path) then
            return
        end
        add_client({path, '-gocodecompletion'}, {name = 'gols'})
    end

    -- autocommands
    api.nvim_command("autocmd Filetype java lua start_jdt()")
    api.nvim_command("autocmd Filetype haskell lua start_hie()")
    api.nvim_command("autocmd Filetype python lua add_client({'pyls'})")
    api.nvim_command("autocmd Filetype html lua add_client({'html-languageserver', '--stdio'}, {name='html-ls'})")
    api.nvim_command("autocmd Filetype go lua start_go_ls()")
    api.nvim_command("autocmd Filetype sh lua add_client({'bash-language-server', 'start'}, {name = 'bash-ls'})")
    api.nvim_command("autocmd Filetype rust lua add_client({'rls'}, {root={'Cargo.toml', '.git'}})")
    api.nvim_command("autocmd Filetype lua lua add_client({'lua-lsp'})")
    if vim.fn.executable('json-languageserver') then
        api.nvim_command("autocmd Filetype html lua add_client({'json-languageserver', '--stdio'}, {name='json-ls'})")
    end
end

--- @export
return {
    setup = setup;
}
