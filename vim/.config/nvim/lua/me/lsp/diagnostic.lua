local api = vim.api
local M = {}


local function has_errors(diagnostics)
  local found_error = false
  for _, diagnostic in pairs(diagnostics) do
    diagnostic.severity = diagnostic.severity or vim.lsp.protocol.DiagnosticSeverity.Error
    found_error = found_error or diagnostic.severity == vim.lsp.protocol.DiagnosticSeverity.Error
  end
  return found_error
end


local function update_loclist(found_error, result)
  local diagnostics
  if found_error then
    diagnostics = vim.tbl_filter(
      function(x) return x.severity == vim.lsp.protocol.DiagnosticSeverity.Error end,
      result.diagnostics
    )
  else
    diagnostics = result.diagnostics
  end
  vim.fn.setloclist(0, {}, 'r', {
    title = 'Language Server';
    items = vim.lsp.util.diagnostics_to_items({bufnr = diagnostics})
  })
end


if vim.diagnostic then
  function M.publishDiagnostics(err, result, ctx, _)
    local config = {
      virtual_text = false,
    }
    vim.lsp.diagnostic.on_publish_diagnostics(err, result, ctx, config)
    local bufnr = vim.uri_to_bufnr(result.uri)
    if bufnr == api.nvim_get_current_buf() then
      local found_error = has_errors(result.diagnostics)
      local opts = { severity = found_error and vim.diagnostic.severity.ERROR or nil }
      local diagnostics = vim.diagnostic.get(bufnr, opts)
      vim.fn.setloclist(0, {}, 'r', {
        title = 'Language Server',
        items = vim.diagnostic.toqflist(diagnostics)
      })
    end
  end
else
  function M.publishDiagnostics(...)
    local config_or_client_id = select(4, ...)
    local client_id
    local result
    if type(config_or_client_id) == 'number' then
      client_id = config_or_client_id
      result = select(3, ...)
    else
      local ctx = select(3, ...)
      result = select(2, ...)
      client_id = ctx.client_id
    end
    if not result then return end
    local uri = result.uri
    local bufnr = vim.uri_to_bufnr(uri)
    assert(bufnr, "Couldn't get buffer for uri " .. uri)
    vim.lsp.diagnostic.clear(bufnr, client_id)
    local found_error = has_errors(result.diagnostics)
    vim.lsp.diagnostic.save(result.diagnostics, bufnr, client_id)
    if api.nvim_buf_is_loaded(bufnr) then
      vim.lsp.diagnostic.set_signs(result.diagnostics, bufnr, client_id)
    end
    if bufnr == api.nvim_get_current_buf() then
      update_loclist(found_error, result)
    end
  end
end


function M.errors_to_quickfix()
  if vim.diagnostic then
    vim.diagnostic.setqflist { severity = vim.diagnostic.severity.ERROR }
    return
  end
  local items = vim.lsp.util.diagnostics_to_items(
    vim.lsp.diagnostic.get_all(),
    function(d) return d.severity == vim.lsp.protocol.DiagnosticSeverity.Error end
  )
  vim.fn.setqflist({}, 'r', {
    title = 'Language Server';
    items = items
  })
end

function M.warnings_to_quickfix()
  if vim.diagnostic then
    vim.diagnostic.setqflist { severity = vim.diagnostic.severity.WARN }
    return
  end
  local items = vim.lsp.util.diagnostics_to_items(
    vim.lsp.diagnostic.get_all(),
    function(d) return d.severity == vim.lsp.protocol.DiagnosticSeverity.Warning end
  )
  vim.fn.setqflist({}, 'r', {
    title = 'Language Server';
    items = items
  })
end


return M
