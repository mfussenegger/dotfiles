local lsp = require 'vim.lsp'
local api = vim.api
local M = {}
local lspc = {}

do
  local function start_or_attach(config, reuse_client)
    reuse_client = reuse_client or function(client)
      return client.config.root_dir == config.root_dir and client.name == config.name
    end
    local bufnr = api.nvim_get_current_buf()
    for _, client in pairs(vim.lsp.get_active_clients()) do
      if reuse_client(client) then
        lsp.buf_attach_client(bufnr, client.id)
        return
      end
    end
    local client_id = lsp.start_client(config)
    lsp.buf_attach_client(bufnr, client_id)
  end

  function lspc.start(config, root_markers)
    local root_dir = require('jdtls.setup').find_root(root_markers)
    if not root_dir
      then return
    end
    config['root_dir'] = root_dir
    start_or_attach(config)
  end

  function lspc.restart()
    for _, client in pairs(vim.lsp.get_active_clients()) do
      local bufs = vim.lsp.get_buffers_by_client_id(client.id)
      client.stop()
      local new_client_id = lsp.start_client(client.config)
      for _, buf in pairs(bufs) do
        lsp.buf_attach_client(buf, new_client_id)
      end
    end
  end
  M.restart = lspc.restart
end


-- array of mappings to setup; {<capability>, <mode>, <lhs>, <rhs>}
local key_mappings = {
  {"documentFormattingProvider", "n", "gq", "<Cmd>lua vim.lsp.buf.format{async = true}<CR>"},
  {"documentRangeFormattingProvider", "v", "gq", "<Esc><Cmd>lua vim.lsp.buf.range_formatting()<CR>"},
  {"referencesProvider", "n", "gr", "<Cmd>lua vim.lsp.buf.references()<CR>"},
  {"hoverProvider", "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>"},
  {"implementationProvider", "n", "gD",  "<Cmd>lua vim.lsp.buf.implementation()<CR>"},
  {"signatureHelpProvider", "i", "<c-space>",  "<Cmd>lua vim.lsp.buf.signature_help()<CR>"},
  {"workspaceSymbolProvider", "n", "gW", "<Cmd>lua vim.lsp.buf.workspace_symbol()<CR>"},
  {"codeActionProvider", "n", "<a-CR>", "<Cmd>lua vim.lsp.buf.code_action()<CR>"},
  {"codeActionProvider", "n", "<leader>r", "<Cmd>lua vim.lsp.buf.code_action { context = { only = {'refactor'} }}<CR>"},
  {"codeActionProvider", "v", "<a-CR>", "<Esc><Cmd>lua vim.lsp.buf.range_code_action()<CR>"},
  {"codeActionProvider", "v", "<leader>r", "<Esc><Cmd>lua vim.lsp.buf.range_code_action { context = { only = {'refactor'}}}<CR>"},
}


local function on_init(client)
  if client.config.settings then
    client.notify('workspace/didChangeConfiguration', { settings = client.config.settings })
  end
end

local function on_attach(client, bufnr, attach_opts)
  require('lsp_compl').attach(client, bufnr, attach_opts)
  api.nvim_buf_set_var(bufnr, "lsp_client_id", client.id)
  api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  api.nvim_buf_set_option(bufnr, "bufhidden", "hide")

  if client.server_capabilities.definitionProvider then
    api.nvim_buf_set_option(bufnr, 'tagfunc', "v:lua.vim.lsp.tagfunc")
  end
  local opts = { silent = true; }
  for _, mappings in pairs(key_mappings) do
    local capability, mode, lhs, rhs = unpack(mappings)
    if client.server_capabilities[capability] then
      api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
    end
  end
  api.nvim_buf_set_keymap(bufnr, "n", "crr", "<Cmd>lua vim.lsp.buf.rename(vim.fn.input('New Name: '))<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "i", "<c-n>", "<Cmd>lua require('lsp_compl').trigger_completion()<CR>", opts)
  vim.cmd('augroup lsp_aucmds')
  vim.cmd(string.format('au! * <buffer=%d>', bufnr))
  if client.server_capabilities.documentHighlightProvider then
    vim.cmd(string.format('au CursorHold  <buffer=%d> lua vim.lsp.buf.document_highlight()', bufnr))
    vim.cmd(string.format('au CursorHoldI <buffer=%d> lua vim.lsp.buf.document_highlight()', bufnr))
    vim.cmd(string.format('au CursorMoved <buffer=%d> lua vim.lsp.buf.clear_references()', bufnr))
  end
  if vim.lsp.codelens and client.server_capabilities.codeLensProvider then
    api.nvim_buf_set_keymap(bufnr, "n", "<leader>cr", "<Cmd>lua vim.lsp.codelens.refresh()<CR>", opts)
    api.nvim_buf_set_keymap(bufnr, "n", "<leader>ce", "<Cmd>lua vim.lsp.codelens.run()<CR>", opts)
  end
  vim.cmd('augroup end')
end
M.on_attach = on_attach


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
    init_options = {},
    settings = {},
  }
end
M.mk_config = mk_config


function M.add_client(cmd, config)
  local default_config = mk_config()
  config = vim.tbl_deep_extend('force', default_config, config or {})
  config.cmd = cmd
  config.name = config.name or cmd[1]
  local root_markers = config.root or {'.git'}
  config.root = nil
  lspc.start(config, root_markers)
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
    client.server_capabilities.hoverProvider = false
    on_attach(client, bufnr)
  end
  lspc.start(config, {'.git'})
end


function M.start_yaml_ls()
  local name = api.nvim_buf_get_name(0)
  local ansible_patterns = {
    '.*/playbooks/.*%.yml',
    '.*/roles/.*%.yml',
    '.*/playbooks/.*%.yaml',
    '.*/roles/.*%.yaml',
  }
  for _, pattern in pairs(ansible_patterns) do
    if name:match(pattern) then
      return
    end
  end
  M.add_client({'yaml-language-server', '--stdio'})
end


function M.setup()
  vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, { border = 'single' })
  vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = 'single' })
end


return M
