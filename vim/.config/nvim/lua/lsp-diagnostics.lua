local myutil = require 'util'
local api = vim.api
local timer = nil
local M = {}

local updatedDiagnostics = false
local nextDiag = 0


local function diagnostics_to_items(diagnostics)
  local items = {}
  if not diagnostics then return items end
  local diagnostics_by_line = vim.lsp.util.diagnostics_group_by_line(diagnostics)
  for linenr, line_diagnostics in pairs(diagnostics_by_line) do
    if #line_diagnostics > 0 then
      local d = line_diagnostics[1]
      table.insert(items, {
        bufnr = d.bufnr,
        lnum = linenr + 1,
        vcol = 1,
        col = d.range.start.character + 1,
        text = d.message
      })
    end
  end
  return items
end


do
  local function refresh_diagnostics()
    if timer then
      timer:stop()
      timer:close()
      timer = nil
    end
    timer = vim.loop.new_timer()
    timer:start(450, 0, vim.schedule_wrap(function()
      if timer then
        timer:close()
        timer = nil
      end
      if updatedDiagnostics then
        updatedDiagnostics = false
        nextDiag = 0
      end
    end))
  end

  function M.publishDiagnostics(_, _, result)
    if not result then return end
    local uri = result.uri
    local bufnr = vim.uri_to_bufnr(uri)
    if not bufnr then
      myutil.err_message("LSP.publishDiagnostics: Couldn't find buffer for ", uri)
      return
    end
    vim.lsp.util.buf_clear_diagnostics(bufnr)
    if not api.nvim_buf_is_loaded(bufnr) then
      return
    end
    local has_errors = false
    for _, diagnostic in ipairs(result.diagnostics) do
      diagnostic.severity = diagnostic.severity or vim.lsp.protocol.DiagnosticSeverity.Error
      diagnostic.bufnr = bufnr
      diagnostic.uri = uri
      has_errors = has_errors or diagnostic.severity == vim.lsp.protocol.DiagnosticSeverity.Error
    end
    vim.lsp.util.buf_diagnostics_save_positions(bufnr, result.diagnostics)
    vim.lsp.util.buf_diagnostics_signs(bufnr, result.diagnostics)
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
        items = diagnostics_to_items(diagnostics)
      })
    end
    updatedDiagnostics = true
    refresh_diagnostics()
  end
end

function M.errors_to_quickfix()
  local diagnostics = {}
  for _, bufdiagnostics in pairs(vim.lsp.util.diagnostics_by_buf) do
    for _, d in pairs(bufdiagnostics) do
      if d.severity == vim.lsp.protocol.DiagnosticSeverity.Error then
        table.insert(diagnostics, d)
      end
    end
  end
  vim.fn.setqflist({}, 'r', {
    title = 'Language Server';
    items = diagnostics_to_items(diagnostics)
  })
end

function M.warnings_to_quickfix()
  local diagnostics = {}
  for _, bufdiagnostics in pairs(vim.lsp.util.diagnostics_by_buf) do
    for _, d in pairs(bufdiagnostics) do
      if d.severity == vim.lsp.protocol.DiagnosticSeverity.Warning then
        table.insert(diagnostics, d)
      end
    end
  end
  vim.fn.setqflist({}, 'r', {
    title = 'Language Server';
    items = diagnostics_to_items(diagnostics)
  })
end


local function diagnostics_sorted_by_severity()
  local all_diagnostics = {}
  for _, diagnostics in pairs(vim.lsp.util.diagnostics_by_buf) do
    local last_col = nil
    for _, d in pairs(diagnostics) do
      local fname = vim.uri_to_fname(d.uri)
      local stat = vim.loop.fs_stat(fname)
      local is_dir = stat and stat.type == 'directory'
      if not is_dir and (last_col == nil or last_col ~= d.range.start.character) then
        last_col = d.range.start.character
        table.insert(all_diagnostics, d)
      end
    end
  end
  table.sort(all_diagnostics, function(a, b)
    if a.severity == b.severity then
      return a.range.start.line < b.range.start.line
    else
      return a.severity < b.severity
    end
  end)
  return all_diagnostics
end


function M.next_diag()
    local diagnostics = diagnostics_sorted_by_severity()
    if #diagnostics == 0 then
        return
    end
    nextDiag = nextDiag + 1
    if nextDiag > #diagnostics then
        nextDiag = 1
    end
    local d = diagnostics[nextDiag]
    local bufnr = vim.uri_to_bufnr(d.uri)
    myutil.jump_to_buf(bufnr, d.range)
end


function M.prev_diag()
    local diagnostics = diagnostics_sorted_by_severity()
    if #diagnostics == 0 then
        return
    end
    nextDiag = nextDiag - 1
    if nextDiag < 1 then
        nextDiag = #diagnostics
    end
    local d = diagnostics[nextDiag]
    local bufnr = vim.uri_to_bufnr(d.uri)
    myutil.jump_to_buf(bufnr, d.range)
end


return M
