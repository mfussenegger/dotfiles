local util = vim.lsp.util
local api = vim.api
local M = {}
local lens_cache_by_buf = {}
local active_requests = {}
local namespaces = setmetatable({}, {
  __index = function(t, key)
    local value = api.nvim_create_namespace('vim_lsp_codelens:' .. key)
    rawset(t, key, value)
    return value
  end;
})


function M.run()
  local line = api.nvim_win_get_cursor(0)[1]
  local options = {}
  local lenses_by_client = lens_cache_by_buf[api.nvim_get_current_buf()] or {}
  -- todo: utilize get
  for _, lenses in pairs(lenses_by_client) do
    for _, lens in pairs(lenses) do
      if lens.command and lens.range.start.line == (line - 1) then
        table.insert(options, lens)
      end
    end
  end
  if #options == 0 then
    print('No executable codelens found at current line')
  elseif #options == 1 then
    vim.lsp.buf.execute_command(options[1].command)
  else
    -- todo: change this to inputlist for upstreaming
    require('fzy').pick_one(
      options,
      'Lens> ',
      function(option) return option.command end,
      function(option)
        vim.lsp.buf.execute_command(option.command)
      end
    )
  end
end


-- todo: expose get(bufnr, client_id, line) ?
-- todo: expose get_all() ?


local function resolve_lens(cache, idx, lens)
  -- If the chandler should be overridable in vim.lsp.handlers the structure of the cache would need to change
  -- So that the closure is not necessary
  -- Maybe {bufnr -> client_id -> linenr -> lenses}?
  local _, cancel = vim.lsp.buf_request(0, 'codeLens/resolve', lens, function(err, _, result, client_id, bufnr)
    assert(not err, vim.inspect(err))
    if not result then return end
    if result.command then
      local line = result.range.start.line
      local chunks = { {result.command.title, 'LspCodeLens'} }
      local ns = namespaces[client_id]
      api.nvim_buf_set_virtual_text(bufnr, ns, line, chunks, {})
    end
    cache[idx] = result
  end)
  table.insert(active_requests, cancel)
end


function M.on_codelens(err, _, result, client_id, bufnr)
  assert(not err, vim.inspect(err))
  -- todo, window could have changed
  local wininfo = vim.fn.getwininfo(api.nvim_get_current_win())[1]
  local lenses = {}
  local ns = namespaces[client_id]
  -- Can we avoid flickering? should we track which lens disappeared somehow?
  api.nvim_buf_clear_namespace(bufnr, ns, 1, -1)
  for idx, lens in pairs(result or {}) do
    lenses[idx] = lens
    if lens.command then
      local line = lens.range.start.line
      local chunks = { {lens.command.title, 'LspCodeLens'} }
      api.nvim_buf_set_virtual_text(bufnr, ns, line, chunks, {})
    else
      local within_viewport = (
        wininfo.topline < lens.range.start.line
        and wininfo.botline > lens.range.start.line
      )
      if within_viewport then
        resolve_lens(lenses, idx, lens)
      end
    end
  end
  local lenses_by_client = lens_cache_by_buf[bufnr]
  if not lenses_by_client then
    lenses_by_client = {}
    lens_cache_by_buf[bufnr] = lenses_by_client
    api.nvim_buf_attach(bufnr, false, {
      on_detach = function(b) lens_cache_by_buf[b] = nil end
    })
  end
  lenses_by_client[client_id] = lenses
end


function M.refresh()
  -- TODO: debounce?
  local params = {
    textDocument = util.make_text_document_params()
  }
  for _, cancel_req in pairs(active_requests) do
    cancel_req()
  end
  active_requests = {}
  local _, cancel = vim.lsp.buf_request(0, 'textDocument/codeLens', params, M.on_codelens)
  table.insert(active_requests, cancel)
end


return M
