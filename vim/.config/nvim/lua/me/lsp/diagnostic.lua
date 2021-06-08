local api = vim.api
local M = {}


do
  function M.publishDiagnostics(_, _, result, client_id)
    if not result then return end
    local uri = result.uri
    local bufnr = vim.uri_to_bufnr(uri)
    assert(bufnr, "Couldn't get buffer for uri " .. uri)
    vim.lsp.diagnostic.clear(bufnr, client_id)
    local has_errors = false
    for _, diagnostic in ipairs(result.diagnostics) do
      diagnostic.severity = diagnostic.severity or vim.lsp.protocol.DiagnosticSeverity.Error
      has_errors = has_errors or diagnostic.severity == vim.lsp.protocol.DiagnosticSeverity.Error
    end
    vim.lsp.diagnostic.save(result.diagnostics, bufnr, client_id)
    if api.nvim_buf_is_loaded(bufnr) then
      vim.lsp.diagnostic.set_signs(result.diagnostics, bufnr, client_id)
    end
    if bufnr == api.nvim_get_current_buf() then
      local diagnostics
      if has_errors then
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
  end
end


function M.errors_to_quickfix()
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
