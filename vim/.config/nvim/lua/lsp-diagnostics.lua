local myutil = require 'util'
local api = vim.api
local M = {}


local function diagnostics_to_items(diagnostics_by_buf, predicate)
  if not diagnostics_by_buf then
    return {}
  end
  local items = {}
  for bufnr, diagnostics in pairs(diagnostics_by_buf) do
    for _, d in pairs(diagnostics) do
      if not predicate or predicate(d) then
        table.insert(items, {
          bufnr = bufnr,
          lnum = d.range.start.line + 1,
          col = d.range.start.character + 1,
          text = d.message,
          vcol = 1,
        })
      end
    end
  end
  table.sort(items, function(a, b) return a.lnum < b.lnum end)
  return items
end


do
  function M.publishDiagnostics(_, _, result, client_id)
    if not result then return end
    local uri = result.uri
    local bufnr = vim.uri_to_bufnr(uri)
    if not bufnr then
      myutil.err_message("LSP.publishDiagnostics: Couldn't find buffer for ", uri)
      return
    end
    vim.lsp.diagnostic.clear(bufnr)
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
        items = diagnostics_to_items({bufnr = diagnostics})
      })
    end
  end
end


function M.errors_to_quickfix()
  local items = diagnostics_to_items(
    vim.lsp.diagnostic.get_all(),
    function(d) return d.severity == vim.lsp.protocol.DiagnosticSeverity.Error end
  )
  vim.fn.setqflist({}, 'r', {
    title = 'Language Server';
    items = items
  })
end

function M.warnings_to_quickfix()
  local items = diagnostics_to_items(
    vim.lsp.diagnostic.get_all(),
    function(d) return d.severity == vim.lsp.protocol.DiagnosticSeverity.Warning end
  )
  vim.fn.setqflist({}, 'r', {
    title = 'Language Server';
    items = items
  })
end


return M
