local myutil = require 'util'
local lsp = require 'vim.lsp'
local lsp_ext = require 'lsp-ext'
local lsp_diag = require 'lsp-diagnostics'
local jdtls = require 'jdtls'
local api = vim.api

-- id is filetype│root_dir
local lsps = {}


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
    local lsp_id = tostring(vim.bo.filetype) .. "│" .. root_dir
    local client_id = lsps[lsp_id]
    if not client_id then
        client_id = lsp.start_client(config)
        lsps[lsp_id] = client_id
    end
    lsp.buf_attach_client(bufnr, client_id)
end


-- array of mappings to setup; {<capability_name>, <mode>, <lhs>, <rhs>}
local key_mappings = {
    {"code_action", "n", "<a-CR>", "<Cmd>lua require'jdtls'.code_action()<CR>"},
    {"code_action", "n", "<leader>r", "<Cmd>lua require'jdtls'.code_action(false, 'refactor')<CR>"},
    {"code_action", "v", "<a-CR>", "<Esc><Cmd>lua require'jdtls'.code_action(true)<CR>"},
    {"code_action", "v", "<leader>r", "<Esc><Cmd>lua require'jdtls'.code_action(true, 'refactor')<CR>"},

    {"document_formatting", "n", "gq", "<Cmd>lua vim.lsp.buf.formatting()<CR>"},
    {"document_range_formatting", "v", "gq", "<Esc><Cmd>lua vim.lsp.buf.range_formatting()<CR>"},
    {"find_references", "n", "gr", "<Cmd>lua vim.lsp.buf.references()<CR>"},
    {"goto_definition", "n", "<c-]>",  "<Cmd>lua vim.lsp.buf.definition()<CR>"},
    {"hover", "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>"},
    {"implementation", "n", "gD",  "<Cmd>lua vim.lsp.buf.implementation()<CR>"},
    {"signature_help", "i", "<c-space>",  "<Cmd>lua vim.lsp.buf.signature_help()<CR>"},
    {"workspace_symbol", "n", "gW", "<Cmd>lua vim.lsp.buf.workspace_symbol()<CR>"}
}

local function on_init(client, _)
    api.nvim_command("augroup LspExt")
    api.nvim_command("autocmd InsertCharPre * lua require'lsp-ext'._InsertCharPre()")
    api.nvim_command("autocmd InsertLeave * lua require'lsp-ext'._InsertLeave()")
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
    api.nvim_buf_set_keymap(bufnr, "n", "<space>", "<Cmd>lua vim.lsp.util.show_line_diagnostics()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "crr", "<Cmd>lua vim.lsp.buf.rename()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "]w", "<Cmd>lua require'lsp-diagnostics'.next_diag()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "[w", "<Cmd>lua require'lsp-diagnostics'.prev_diag()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "i", "<c-n>", "<Cmd>lua require('lsp-ext').trigger_completion()<CR>", opts)
    if client.resolved_capabilities['document_highlight'] then
      api.nvim_command [[autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()]]
      api.nvim_command [[autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()]]
      api.nvim_command [[autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()]]
    end
end

local function jdtls_on_attach(client, bufnr)
    on_attach(client, bufnr)
    local opts = { silent = true; }
    jdtls.setup_dap()
    api.nvim_buf_set_keymap(bufnr, "n", "<A-o>", "<Cmd>lua require'jdtls'.organize_imports()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "<leader>df", "<Cmd>lua require'jdtls'.test_class()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "<leader>dn", "<Cmd>lua require'jdtls'.test_nearest_method()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "v", "crv", "<Esc><Cmd>lua require('jdtls').extract_variable(true)<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "crv", "<Cmd>lua require('jdtls').extract_variable()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "v", "crm", "<Esc><Cmd>lua require('jdtls').extract_method(true)<CR>", opts)

    api.nvim_command [[command! -buffer -nargs=? JdtCompile lua require('jdtls').compile(<f-args>)]]
    api.nvim_command [[command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()]]
    api.nvim_command [[command! -buffer -nargs=* JdtJol lua require('jdtls').jol(<f-args>)]]
    api.nvim_command [[command! -buffer JdtBytecode lua require('jdtls').javap()]]
    api.nvim_command [[command! -buffer JdtJshell lua require('jdtls').jshell()]]
end

local function mk_config()
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities.textDocument.completion.completionItem.snippetSupport = true
    capabilities.textDocument.codeAction = {
        dynamicRegistration = false;
        codeActionLiteralSupport = {
            codeActionKind = {
                valueSet = {
                    "source.generate.toString",
                    "source.generate.hashCodeEquals",
                    "source.organizeImports",
                };
            };
        };
    }
    return {
        callbacks = {
            ["textDocument/publishDiagnostics"] = lsp_diag.publishDiagnostics,
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


local function attach_to_active_buf(bufnr)
  local client
  for _, buf in pairs(vim.fn.getbufinfo({bufloaded=true})) do
    if api.nvim_buf_get_option(buf.bufnr, 'filetype') == 'java' then
      local clients = vim.lsp.buf_get_clients(buf.bufnr)
      if #clients > 0 then
        client = clients[1]
        break
      end
    end
  end
  if not client then
    print('No active LSP client found to use for jdt:// document')
    return false
  end
  lsp.buf_attach_client(bufnr, client.id)
  return true
end


function M.start_jdt()
    local bufnr = api.nvim_get_current_buf()
    local bufname = api.nvim_buf_get_name(bufnr)
    -- jdt paths are returned by eclipse.jdt.ls and can be opened using the nvim-jdtls plugin
    -- Just attach to the already running client, otherwise the root_pattern logic below would bail out
    if vim.startswith(bufname, 'jdt://') then
      if attach_to_active_buf(bufnr) then
        return
      end
    end
    local lsp4j_status_callback = vim.schedule_wrap(function(_, _, result)
        api.nvim_command(string.format(':echohl Function | echo "%s" | echohl None', result.message))
    end)
    local config = mk_config()
    config['name'] = 'jdt.ls'
    local root_markers = {'gradlew', '.git'}
    local root_dir = myutil.root_pattern(bufnr, root_markers)
    if not root_dir then
        return
    end
    local workspace_folder = os.getenv("HOME") .. "/.local/share/eclipse/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
    config['cmd'] = {'java-lsp.sh', workspace_folder}
    config['callbacks']["language/status"] = lsp4j_status_callback

    local home = os.getenv('HOME')
    local bundles = {
        vim.fn.glob(home .. '/dev/microsoft/java-debug/com.microsoft.java.debug.plugin/target/com.microsoft.java.debug.plugin-*.jar'),
    }
    vim.list_extend(bundles, vim.split(vim.fn.glob(home .. "/dev/microsoft/vscode-java-test/server/*.jar"), "\n"))
    config['init_options'] = {
        settings = {
            java = {
                signatureHelp = { enabled = true };
                completion = {
                    favoriteStaticMembers = {
                        "org.hamcrest.MatcherAssert.assertThat",
                        "org.hamcrest.Matchers.*",
                        "org.hamcrest.CoreMatchers.*",
                        "java.util.Objects.requireNonNull",
                        "java.util.Objects.requireNonNullElse"
                    }
                };
                codeGeneration = {
                    toString = {
                        template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}"
                    }
                }
            };
        };
        bundles = bundles;
        extendedClientCapabilities = require('jdtls').extendedClientCapabilities;
    }
    config['on_attach'] = jdtls_on_attach
    add_client_by_cfg(config, root_markers)
end
function M.start_hie()
    local config = mk_config()
    config['name'] = 'hls'
    config['cmd'] = {'haskell-language-server-wrapper', '--lsp'}
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
