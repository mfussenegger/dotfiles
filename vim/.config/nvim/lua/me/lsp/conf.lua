local lsp = require 'vim.lsp'
local jdtls = require 'jdtls'
local api = vim.api
local M = {}
local lspc = {}

do
  -- id is filetype│root_dir
  local lsp_client_ids = {}

  function lspc.start(config, root_markers)
    local root_dir = require('jdtls.setup').find_root(root_markers)
    if not root_dir then return end

    local cmd = config.cmd[1]
    if tonumber(vim.fn.executable(cmd)) == 0 then
      api.nvim_command(string.format(
        ':echohl WarningMsg | redraw | echo "No LSP executable: %s" | echohl None', cmd))
      return
    end
    config['root_dir'] = root_dir
    local lsp_id = tostring(vim.bo.filetype) .. "│" .. root_dir
    local client_id = lsp_client_ids[lsp_id]
    if not client_id then
      client_id = lsp.start_client(config)
      lsp_client_ids[lsp_id] = client_id
    end
    local bufnr = api.nvim_get_current_buf()
    lsp.buf_attach_client(bufnr, client_id)
  end

  function lspc.restart()
    for id, client_id in pairs(lsp_client_ids) do
      local client = vim.lsp.get_client_by_id(client_id)
      if client then
        local bufs = vim.lsp.get_buffers_by_client_id(client_id)
        client.stop()
        local new_client_id = lsp.start_client(client.config)
        lsp_client_ids[id] = new_client_id
        for _, buf in pairs(bufs) do
          lsp.buf_attach_client(buf, new_client_id)
        end
      end
    end
  end
  M.restart = lspc.restart
end


-- array of mappings to setup; {<capability_name>, <mode>, <lhs>, <rhs>}
local key_mappings = {
  {"document_formatting", "n", "gq", "<Cmd>lua vim.lsp.buf.formatting()<CR>"},
  {"document_range_formatting", "v", "gq", "<Esc><Cmd>lua vim.lsp.buf.range_formatting()<CR>"},
  {"find_references", "n", "gr", "<Cmd>lua vim.lsp.buf.references()<CR>"},
  {"hover", "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>"},
  {"implementation", "n", "gD",  "<Cmd>lua vim.lsp.buf.implementation()<CR>"},
  {"signature_help", "i", "<c-space>",  "<Cmd>lua vim.lsp.buf.signature_help()<CR>"},
  {"workspace_symbol", "n", "gW", "<Cmd>lua vim.lsp.buf.workspace_symbol()<CR>"},
  {"code_action", "n", "<a-CR>", "<Cmd>lua vim.lsp.buf.code_action()<CR>"},
  {"code_action", "n", "<leader>r", "<Cmd>lua vim.lsp.buf.code_action { only = 'refactor' }<CR>"},
  {"code_action", "v", "<a-CR>", "<Esc><Cmd>lua vim.lsp.buf.range_code_action()<CR>"},
  {"code_action", "v", "<leader>r", "<Esc><Cmd>lua vim.lsp.buf.range_code_action { only = 'refactor'}<CR>"},
}


local function on_init(client)
  lsp.util.text_document_completion_list_to_complete_items = require('lsp_compl').text_document_completion_list_to_complete_items
  if client.config.settings then
    client.notify('workspace/didChangeConfiguration', { settings = client.config.settings })
  end
end

local function on_attach(client, bufnr, attach_opts)
  require('lsp_compl').attach(client, bufnr, attach_opts)
  api.nvim_buf_set_var(bufnr, "lsp_client_id", client.id)
  api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  api.nvim_buf_set_option(bufnr, "bufhidden", "hide")

  if client.resolved_capabilities.goto_definition then
    if vim.lsp.tagfunc then
      api.nvim_buf_set_option(bufnr, 'tagfunc', "v:lua.vim.lsp.tagfunc")
    else
      api.nvim_buf_set_option(bufnr, 'tagfunc', "v:lua.require'me.lsp.ext'.tagfunc")
    end
  end
  local opts = { silent = true; }
  for _, mappings in pairs(key_mappings) do
    local capability, mode, lhs, rhs = unpack(mappings)
    if client.resolved_capabilities[capability] then
      api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
    end
  end
  api.nvim_buf_set_keymap(bufnr, "n", "crr", "<Cmd>lua vim.lsp.buf.rename(vim.fn.input('New Name: '))<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "i", "<c-n>", "<Cmd>lua require('lsp_compl').trigger_completion()<CR>", opts)
  vim.cmd('augroup lsp_aucmds')
  vim.cmd(string.format('au! * <buffer=%d>', bufnr))
  if client.resolved_capabilities['document_highlight'] then
    vim.cmd(string.format('au CursorHold  <buffer=%d> lua vim.lsp.buf.document_highlight()', bufnr))
    vim.cmd(string.format('au CursorHoldI <buffer=%d> lua vim.lsp.buf.document_highlight()', bufnr))
    vim.cmd(string.format('au CursorMoved <buffer=%d> lua vim.lsp.buf.clear_references()', bufnr))
  end
  if vim.lsp.codelens and client.resolved_capabilities['code_lens'] then
    -- vim.cmd(string.format('au BufEnter,BufModifiedSet,InsertLeave <buffer=%d> lua vim.lsp.codelens.refresh()', bufnr))
    api.nvim_buf_set_keymap(bufnr, "n", "<leader>cr", "<Cmd>lua vim.lsp.codelens.refresh()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "<leader>ce", "<Cmd>lua vim.lsp.codelens.run()<CR>", opts)
  end
  vim.cmd('augroup end')
end

local function jdtls_on_attach(client, bufnr)
  on_attach(client, bufnr, {
    server_side_fuzzy_completion = true,
    trigger_on_delete = false
  })
  local opts = { silent = true; }
  jdtls.setup_dap({hotcodereplace = 'auto'})
  jdtls.setup.add_commands()
  local nnoremap = function(lhs, rhs)
    api.nvim_buf_set_keymap(bufnr, 'n', lhs, rhs, opts)
  end
  nnoremap("<A-o>", "<Cmd>lua require'jdtls'.organize_imports()<CR>")
  nnoremap("<leader>df", "<Cmd>lua require('me.dap'); require'jdtls'.test_class()<CR>")
  nnoremap("<leader>dn", "<Cmd>lua require('me.dap'); require'jdtls'.test_nearest_method()<CR>")
  nnoremap("crv", "<Esc><Cmd>lua require('jdtls').extract_variable(true)<CR>")
  nnoremap("crv", "<Cmd>lua require('jdtls').extract_variable()<CR>")
  nnoremap("crm", "<Esc><Cmd>lua require('jdtls').extract_method(true)<CR>")
  nnoremap("crc", "<Esc><Cmd>lua require('jdtls').extract_constant(true)<CR>")
  nnoremap("crc", "<Cmd>lua require('jdtls').extract_constant()<CR>")
end


local function on_exit(client, bufnr)
  require('me.lsp.ext').detach(client.id, bufnr)
  vim.cmd('augroup lsp_aucmds')
  vim.cmd(string.format('au! * <buffer=%d>', bufnr))
  vim.cmd('augroup end')
end


local function mk_config()
  local capabilities = lsp.protocol.make_client_capabilities()
  capabilities.workspace.configuration = true
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  return {
    flags = {
      debounce_text_changes = 80,
      allow_incremental_sync = true,
    };
    handlers = {},
    capabilities = capabilities;
    on_init = on_init;
    on_attach = on_attach;
    on_exit = on_exit;
  }
end


function M.add_client(cmd, opts)
  local config = mk_config()
  config['name'] = opts and opts.name or cmd[1]
  config['cmd'] = cmd
  lspc.start(config, opts and opts.root or {'.git'})
end


function M.start_jdt()
  local root_markers = {'gradlew', '.git'}
  local root_dir = require('jdtls.setup').find_root(root_markers)
  local home = os.getenv('HOME')
  local workspace_folder = home .. "/.local/share/eclipse/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
  local config = mk_config()
  config.flags.server_side_fuzzy_completion = true
  config.settings = {
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
        },
        useBlocks = true,
      };
      configuration = {
        runtimes = {
          {
            name = "JavaSE-1.8",
            path = "/usr/lib/jvm/java-8-openjdk/",
          },
          {
            name = "JavaSE-11",
            path = "/usr/lib/jvm/java-11-openjdk/",
          },
          {
            name = "JavaSE-16",
            path = home .. "/.local/jdks/jdk-16.0.1+9/",
          },
          {
            name = "JavaSE-17",
            path = "/usr/lib/jvm/java-17-openjdk/",
          },
        }
      };
    };
  }
  config.cmd = {
    '/usr/lib/jvm/java-17-openjdk/bin/java',
    '-Declipse.application=org.eclipse.jdt.ls.core.id1',
    '-Dosgi.bundles.defaultStartLevel=4',
    '-Declipse.product=org.eclipse.jdt.ls.core.product',
    '-Dlog.protocol=true',
    '-Dlog.level=ALL',
    '-Xms1g',
    '--add-modules=ALL-SYSTEM',
    '--add-opens', 'java.base/java.util=ALL-UNNAMED',
    '--add-opens', 'java.base/java.lang=ALL-UNNAMED',
    '-jar', vim.fn.glob(home .. '/dev/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_*.jar'),
    '-configuration', home .. '/dev/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/config_linux',
    '-data', workspace_folder,
  }
  config.on_attach = jdtls_on_attach

  local jar_patterns = {
    '/dev/microsoft/java-debug/com.microsoft.java.debug.plugin/target/com.microsoft.java.debug.plugin-*.jar',
    '/dev/dgileadi/vscode-java-decompiler/server/*.jar',
    '/dev/microsoft/vscode-java-test/java-extension/com.microsoft.java.test.plugin/target/*.jar',
    '/dev/microsoft/vscode-java-test/java-extension/com.microsoft.java.test.runner/target/*.jar',
    '/dev/microsoft/vscode-java-test/java-extension/com.microsoft.java.test.runner/lib/*.jar',
    '/dev/testforstephen/vscode-pde/server/*.jar'
  }
  -- npm install broke for me: https://github.com/npm/cli/issues/2508
  -- So gather the required jars manually; this is based on the gulpfile.js in the vscode-java-test repo
  local plugin_path = '/dev/microsoft/vscode-java-test/java-extension/com.microsoft.java.test.plugin.site/target/repository/plugins/'
  local bundle_list = vim.tbl_map(
    function(x) return require('jdtls.path').join(plugin_path, x) end,
    {
      'org.eclipse.jdt.junit4.runtime_*.jar',
      'org.eclipse.jdt.junit5.runtime_*.jar',
      'org.junit.jupiter.api*.jar',
      'org.junit.jupiter.engine*.jar',
      'org.junit.jupiter.migrationsupport*.jar',
      'org.junit.jupiter.params*.jar',
      'org.junit.vintage.engine*.jar',
      'org.opentest4j*.jar',
      'org.junit.platform.commons*.jar',
      'org.junit.platform.engine*.jar',
      'org.junit.platform.launcher*.jar',
      'org.junit.platform.runner*.jar',
      'org.junit.platform.suite.api*.jar',
      'org.apiguardian*.jar'
    }
  )
  vim.list_extend(jar_patterns, bundle_list)
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
    bundles = bundles;
    extendedClientCapabilities = extendedClientCapabilities;
  }
  -- mute; having progress reports is enough
  config.handlers['language/status'] = function() end
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
  config.on_attach = function(client, bufnr)
    on_attach(client, bufnr, { server_side_fuzzy_completion = true })
  end
  lspc.start(config, {'hie.yaml', 'stack.yml', '.git'})
end


function M.gopls()
  local config = mk_config()
  config.name = 'gopls'
  config.cmd = {'gopls', 'serve'}
  config.settings = {
    gopls = {
      experimentalPostfixCompletions = true,
      codelenses = {
        generate = true,
        gc_details = true,
        test = true,
        tidy = true,
      },
    },
  }
  lspc.start(config, {'.git'})
end


function M.start_lua_ls()
  local config = mk_config()
  local library = {}
  local path = vim.split(package.path, ";")
  table.insert(path, "lua/?.lua")
  table.insert(path, "lua/?/init.lua")

  local function add(lib)
    for _, p in pairs(vim.fn.expand(lib, false, true)) do
      p = vim.loop.fs_realpath(p)
      library[p] = true
    end
  end

  add("$VIMRUNTIME")
  add("~/.config/nvim")
  add("~/.config/nvim/pack/plugins/start/*")
  config.settings = {
    Lua = {
      diagnostics = {
        globals = {'vim', 'it', 'describe'}
      },
      runtime = {
        version = "LuaJIT",
        path = path,
      },
      workspace = {
        library = library,
      },
      telemetry = {
        enable = false,
      },
    }
  }
  local server_dir = vim.fn.expand('~/dev/sumneko/lua-language-server/')
  config['name'] = 'luals'
  config['cmd'] = {server_dir .. 'bin/Linux/lua-language-server', server_dir .. 'main.lua'}
  lspc.start(config, {'.git'})
end


function M.start_omnisharp()
  local pid = vim.fn.getpid()
  local path = vim.fn.expand('~/apps/omnisharp/run')
  local opts = {
    name = 'omnisharp',
    root = {'.csproj', '.git', '.sln'},
  }
  M.add_client({path, '--languageserver', '--hostPID', tostring(pid)}, opts)
end


function M.start_ansible_ls()
  local config = mk_config()
  config.name = 'ansible-ls'
  config.cmd = {'node', os.getenv('HOME') .. '/dev/ansible/ansible-language-server/out/server/src/server.js', '--stdio'}
  config.settings = {
    ansible = {
      ansible = {
        path = '/usr/bin/ansible',
      },
      python = {
        interpreterPath = '/usr/bin/python',
      },
      executionEnvironment = {
        enabled = false,
      },
      ansibleLint = {
        enabled = false,
      }
    }
  }
  local activate_path = vim.fn.getcwd() .. '/.venv/bin/activate'
  if vim.loop.fs_stat(activate_path) then
    config.settings.ansible.python.activationScript = activate_path
  end
  config.on_attach = function(client, bufnr)
    -- Keep using ansible-doc via keywordprg its content is more detailed
    client.resolved_capabilities.hover = nil
    on_attach(client, bufnr)
  end
  lspc.start(config, {'.git'})
end


function M.start_yaml_ls()
  local name = api.nvim_buf_get_name(0)
  if name:match('.*/playbooks/.*%.yml') or name:match('.*/roles/.*%.yml') then
    return
  end
  M.add_client({'yaml-language-server', '--stdio'})
end


function M.setup()
  vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, { border = 'single' })
  vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = 'single' })
end


return M
