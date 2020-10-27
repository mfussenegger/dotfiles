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
    api.nvim_command("autocmd CompleteChanged * lua require'lsp-ext'._CompleteChanged()")
    api.nvim_command("augroup end")
    lsp_ext.setup(client)
end

local function on_attach(client, bufnr)
    api.nvim_buf_set_var(bufnr, "lsp_client_id", client.id)
    api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
    api.nvim_buf_set_option(bufnr, "bufhidden", "hide")
    api.nvim_command("setlocal signcolumn=yes")

    local opts = { silent = true; }
    for _, mappings in pairs(key_mappings) do
        local capability, mode, lhs, rhs = unpack(mappings)
        if client.resolved_capabilities[capability] then
            api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
        end
    end
    api.nvim_buf_set_keymap(bufnr, "n", "<space>", "<Cmd>lua vim.lsp.util.show_line_diagnostics()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "crr", "<Cmd>lua vim.lsp.buf.rename(vim.fn.input('New Name: '))<CR>", opts)
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
    jdtls.setup.add_commands()
    api.nvim_buf_set_keymap(bufnr, "n", "<A-o>", "<Cmd>lua require'jdtls'.organize_imports()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "<leader>df", "<Cmd>lua require'jdtls'.test_class()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "<leader>dn", "<Cmd>lua require'jdtls'.test_nearest_method()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "v", "crv", "<Esc><Cmd>lua require('jdtls').extract_variable(true)<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "crv", "<Cmd>lua require('jdtls').extract_variable()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "v", "crm", "<Esc><Cmd>lua require('jdtls').extract_method(true)<CR>", opts)

end

local function mk_config(settings)
  settings = settings or {}
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.workspace.configuration = true
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities.textDocument.codeAction = {
    dynamicRegistration = false,
    codeActionLiteralSupport = {
      codeActionKind = {
        valueSet = {
          'quickfix',
          'refactor',
          'refactor.extract',
          'refactor.inline',
          'refactor.rewrite',
          'source',
          'source.organizeImports'
        }
      }
    }
  }
  return {
    callbacks = {
      ["textDocument/publishDiagnostics"] = lsp_diag.publishDiagnostics,
      ['workspace/configuration'] = function(err, _, params)
        assert(not err, vim.inspect(err))
        local result = {}
        for _, item in ipairs(params.items) do
          if item.section then
            local setting = settings[item.section]
            if setting then
              table.insert(result, setting)
            end
          end
        end
        return result
      end
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
  local root_markers = {'gradlew', '.git'}
  local bufnr = api.nvim_get_current_buf()
  local root_dir = myutil.root_pattern(bufnr, root_markers)
  local home = os.getenv('HOME')
  local workspace_folder = home .. "/.local/share/eclipse/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
  local config = mk_config()
  config.cmd = {'java-lsp.sh', workspace_folder}
  config.on_attach = jdtls_on_attach

  local jar_patterns = {
    '/dev/microsoft/java-debug/com.microsoft.java.debug.plugin/target/com.microsoft.java.debug.plugin-*.jar',
    '/dev/dgileadi/vscode-java-decompiler/server/*.jar',
    '/dev/microsoft/vscode-java-test/server/*.jar',
  }
  local bundles = {}
  for _, jar_pattern in ipairs(jar_patterns) do
    for _, bundle in ipairs(vim.split(vim.fn.glob(home .. jar_pattern), '\n')) do
      if not vim.endswith(bundle, 'com.microsoft.java.test.runner.jar') then
        table.insert(bundles, bundle)
      end
    end
  end
  local extendedClientCapabilities = jdtls.extendedClientCapabilities;
  extendedClientCapabilities.resolveAdditionalTextEditsSupport = true;
  config.init_options = {
    settings = {
      java = {
        signatureHelp = { enabled = true };
        contentProvider = { preferred = 'fernflower' };
        completion = {
          favoriteStaticMembers = {
            "org.hamcrest.MatcherAssert.assertThat",
            "org.hamcrest.Matchers.*",
            "org.hamcrest.CoreMatchers.*",
            "org.junit.jupiter.api.Assertions.*",
            "java.util.Objects.requireNonNull",
            "java.util.Objects.requireNonNullElse",
            "org.mockito.Mockito.*"
          }
        };
        sources = {
          organizeImports = {
            starThreshold = 9999;
            staticStarThreshold = 9999;
          };
        };
        codeGeneration = {
          toString = {
            template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}"
          }
        };
      };
    };
    bundles = bundles;
    extendedClientCapabilities = extendedClientCapabilities;
  }
  jdtls.start_or_attach(config)
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


function M.start_lua_ls()
  local settings = {
    Lua = {
      diagnostics = {
        globals = {'vim', 'it', 'describe'}
      },
      runtime = {version = "LuaJIT"},
      workspace = {
        library = {
          [vim.fn.expand("$VIMRUNTIME/lua")] = true,
          [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
        }
      },
    }
  }
  local config = mk_config(settings)
  local server_dir = vim.fn.expand('~/dev/sumneko/lua-language-server/')
  config['name'] = 'luals'
  config['cmd'] = {server_dir .. 'bin/Linux/lua-language-server', server_dir .. 'main.lua'}
  add_client_by_cfg(config, {'.git'})
end


--- @export
return M
