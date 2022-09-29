local lsp = require 'vim.lsp'
local api = vim.api
local M = {}

function M.mk_config()
  local capabilities = lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
  capabilities.textDocument.completion.contextSupport = true
  capabilities.textDocument.completion.resolveSupport = {
    properties = {'edit', 'documentation', 'detail'},
  }
  return {
    flags = {
      debounce_text_changes = 80,
    };
    handlers = {},
    capabilities = capabilities;
    on_attach = require('lsp_compl').attach;
    init_options = {},
    settings = {},
  }
end

function M.setup()
  vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, { border = 'single' })
  vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = 'single' })
  local servers = {
    {'html', {'vscode-html-language-server', '--stdio'}},
    {'json', {'vscode-json-language-server', '--stdio'}},
    {'css', {'vscode-css-language-server', '--stdio'}},
    {'c', 'clangd', {'.git'}},
    {'sh', {'bash-language-server', 'start'}},
    {'rust', 'rls', {'Cargo.toml', '.git'}},
    {'tex', 'texlab', {'.git'}},
    {'zig', 'zls', {'.git'}},
  }
  local lsp_group = api.nvim_create_augroup('lsp', {})
  for _, server in pairs(servers) do
    api.nvim_create_autocmd('FileType', {
      pattern = server[1],
      group = lsp_group,
      callback = function()
        local cmd = server[2]
        local config = vim.tbl_extend('force', M.mk_config(), {
          name = type(cmd) == "table" and cmd[1] or cmd,
          cmd = type(cmd) == "table" and cmd or {cmd},
        })
        local markers = server[3]
        if markers then
          config.root_dir = vim.fs.dirname(vim.fs.find(markers, { upward = true })[1])
        end
        vim.lsp.start(config)
      end,
    })
  end
  if vim.fn.exists('##LspAttach') ~= 1 then
    return
  end
  local keymap = vim.keymap
  api.nvim_create_autocmd('LspAttach', {
    group = lsp_group,
    callback = function(args)
      -- array of mappings to setup; {<capability>, <mode>, <lhs>, <rhs>}
      local key_mappings = {
        {"referencesProvider", "n", "gr", vim.lsp.buf.references},
        {"hoverProvider", "n", "K", vim.lsp.buf.hover},
        {"implementationProvider", "n", "gD",  vim.lsp.buf.implementation},
        {"signatureHelpProvider", "i", "<c-space>", vim.lsp.buf.signature_help},
        {"workspaceSymbolProvider", "n", "gW", vim.lsp.buf.workspace_symbol},
        {"codeActionProvider", {"n", "v"}, "<a-CR>", vim.lsp.buf.code_action},
        {"codeActionProvider", "n", "<leader>r", "<Cmd>lua vim.lsp.buf.code_action { context = { only = {'refactor'} }}<CR>"},
        {"codeActionProvider", "v", "<leader>r", "<Cmd>lua vim.lsp.buf.code_action { context = { only = {'refactor'}}}<CR>"},
        {"codeLensProvider", "n", "<leader>cr", vim.lsp.codelens.refresh},
        {"codeLensProvider", "n", "<leader>ce", vim.lsp.codelens.run},
      }

      local client = vim.lsp.get_client_by_id(args.data.client_id)
      local group = api.nvim_create_augroup('lsp-' .. args.buf, {})
      keymap.set("n", "crr", "<Cmd>lua vim.lsp.buf.rename(vim.fn.input('New Name: '))<CR>", { buffer = args.buf })
      keymap.set("i", "<c-n>", "<Cmd>lua require('lsp_compl').trigger_completion()<CR>", { buffer = args.buf })
      for _, mappings in pairs(key_mappings) do
        local capability, mode, lhs, rhs = unpack(mappings)
        if client.server_capabilities[capability] then
          keymap.set(mode, lhs, rhs, { buffer = args.buf, silent = true })
        end
      end
      if client.server_capabilities.documentHighlightProvider then
        api.nvim_create_autocmd({'CursorHold', 'CursorHoldI'}, {
          group = group,
          buffer = args.buf,
          callback = function()
            vim.lsp.buf.document_highlight()
          end,
        })
        api.nvim_create_autocmd('CursorMoved', {
          group = group,
          buffer = args.buf,
          callback = function()
            vim.lsp.buf.clear_references()
          end,
        })
      end
    end,
  })
  api.nvim_create_autocmd('LspDetach', {
    group = lsp_group,
    callback = function(args)
      local group_name = 'lsp-' .. args.buf
      pcall(api.nvim_del_augroup_by_name, group_name)
    end,
  })

  api.nvim_create_user_command(
    'LspStop',
    function(kwargs)
      local name = kwargs.fargs[1]
      for _, client in pairs(vim.lsp.get_active_clients()) do
        if client.name == name then
          vim.lsp.stop_client(client.id)
        end
      end
    end,
    {
      nargs = 1,
      complete = function()
        return vim.tbl_map(function(c) return c.name end, vim.lsp.get_active_clients())
      end
    }
  )
end

local function mk_tag_item(name, range, uri)
  local start = range.start
  return {
    name = name,
    filename = vim.uri_to_fname(uri),
    cmd = string.format(
      'call cursor(%d, %d)', start.line + 1, start.character + 1
    )
  }
end

function M.symbol_tagfunc(pattern, flags)
  if not (flags == 'c' or flags == '' or flags == 'i') then
    return vim.NIL
  end
  local clients = vim.lsp.get_active_clients()
  local num_clients = vim.tbl_count(clients)
  local results = {}
  for _, client in pairs(clients) do
    client.request('workspace/symbol', { query = pattern }, function(_, method_or_result, result_or_ctx)
      local result = type(method_or_result) == 'string' and result_or_ctx or method_or_result
      for _, symbol in pairs(result or {}) do
        local loc = symbol.location
        local item = mk_tag_item(symbol.name, loc.range, loc.uri)
        item.kind = (lsp.protocol.SymbolKind[symbol.kind] or 'Unknown')[1]
        table.insert(results, item)
      end
      num_clients = num_clients - 1
    end)
  end
  vim.wait(1500, function() return num_clients == 0 end)
  return results
end



function M.remove_unused_imports()
  vim.diagnostic.setqflist { severity = vim.diagnostic.severity.WARN }
  vim.cmd('packadd cfilter')
  vim.cmd('Cfilter /The import/')
  vim.cmd('cdo normal dd')
  vim.cmd('cclose')
  vim.cmd('wa')
end


local function is_before(x, y)
  if x.start.line < y.start.line then
    return true
  elseif x.start.line == y.start.line then
    return x.start.character < y.start.character
  else
    return false
  end
end

local function move_to_highlight(is_closer)
  local params = lsp.util.make_position_params()
  local win = api.nvim_get_current_win()
  local lnum, col = unpack(api.nvim_win_get_cursor(win))
  lnum = lnum - 1
  local cursor = {
    start = { line = lnum, character = col }
  }
  lsp.buf_request(0, 'textDocument/documentHighlight', params, function(err, result)
    assert(not err, err and err.message)
    local closest = nil
    for _, highlight in pairs(result or {}) do
      local range = highlight.range
      if is_closer(cursor, range) and (closest == nil or is_closer(range, closest)) then
        closest = range
      end
    end
    if closest then
      api.nvim_win_set_cursor(win, { closest.start.line + 1, closest.start.character })
    end
  end)
end

function M.next_highlight()
  return move_to_highlight(is_before)
end


function M.prev_highlight()
  return move_to_highlight(function(x, y) return is_before(y, x) end)
end


return M