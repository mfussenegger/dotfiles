local M = {}
local api = vim.api
local lint_active = {}

function M.statusline()
  local parts = {
    "%<» %{luaeval('U.file_or_lsp_status()')} %h%m%r%=",
    "%#warningmsg#",
    "%{&paste?'[paste] ':''}",
    "%*",

    "%#warningmsg#",
    "%{&ff!='unix'?'['.&ff.'] ':''}",
    "%*",

    "%#warningmsg#",
    "%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.'] ':''}",
    "%*",
    "%{luaeval('U.dap_status()')}"
  }
  local diagnostics
  local bufnr = api.nvim_get_current_buf()
  local has_clients = not vim.tbl_isempty(vim.lsp.buf_get_clients(0))
  if not has_clients and not lint_active[bufnr] then
    diagnostics = {}
  else
    diagnostics = {
      '%#MyStatuslineLSP# ■ ',
      '%#MyStatuslineLSPErrors#%{luaeval("vim.lsp.diagnostic.get_count(0, [[Error]])")}',
      '%#MyStatuslineLSP# □ ',
      '%#MyStaruslineLSPWarnings#%{luaeval("vim.lsp.diagnostic.get_count(0, [[Warning]])")}',
    }
  end
  vim.list_extend(parts, diagnostics)
  if vim.tbl_isempty(diagnostics) then
    table.insert(parts, "%-14.(%l,%c%)")
  else
    table.insert(parts, "%-14.( | %l,%c%)")
  end
  return table.concat(parts, '')
end


function M.dap_status()
  local ok, dap = pcall(require, 'dap')
  if not ok then
    return ''
  end
  local status = dap.status()
  if status ~= '' then
    return status .. ' | '
  end
  return ''
end


function M.file_or_lsp_status()
  local messages = vim.lsp.util.get_progress_messages()
  local mode = api.nvim_get_mode().mode
  if mode ~= 'n' or vim.tbl_isempty(messages) then
    return M.format_uri(vim.uri_from_bufnr(api.nvim_get_current_buf()))
  end
  local percentage
  local result = {}
  for _, msg in pairs(messages) do
    if msg.message then
      table.insert(result, msg.title .. ': ' .. msg.message)
    else
      table.insert(result, msg.title)
    end
    if msg.percentage then
      percentage = math.max(percentage or 0, msg.percentage)
    end
  end
  if percentage then
    return string.format('%03d: %s', percentage, table.concat(result, ', '))
  else
    return table.concat(result, ', ')
  end
end


function M.init_hl()
  local ts = vim.treesitter
  local bufnr = api.nvim_get_current_buf()
  local ft = api.nvim_buf_get_option(bufnr, 'filetype')
  local ok, parser = pcall(ts.get_parser, bufnr, ft)
  if not ok then return end
  local get_query = require('vim.treesitter.query').get_query
  local query
  ok, query = pcall(get_query, ft, 'highlights')
  if ok and query then
    ts.highlighter.new(parser, query)
  end
  api.nvim_buf_attach(bufnr, false, {
    on_detach = function(_, b)
      if ts.highlighter.active[b] then
        ts.highlighter.active[b]:destroy()
      end
    end,
  })
end


function M.enable_lint()
  if not require('lint').linters_by_ft[vim.bo.filetype] then
    return
  end
  local bufnr = api.nvim_get_current_buf()
  lint_active[bufnr] = true
  api.nvim_buf_attach(bufnr, false, {
    on_detach = function(_, b)
      lint_active[b] = nil
    end,
  })
  vim.cmd("augroup lint")
  vim.cmd("au!")
  vim.cmd(string.format("au BufWritePost <buffer=%d> lua require'lint'.try_lint()", bufnr))
  vim.cmd(string.format("au BufEnter <buffer=%d> lua require'lint'.try_lint()", bufnr))
  vim.cmd("augroup end")
  local opts = {
    silent = true;
  }
  api.nvim_buf_set_keymap(bufnr, "n", "]w", "<Cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "[w", "<Cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<space>", "<Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
end


--- Like :only but delete other buffers
function M.only()
  local cur_buf = api.nvim_get_current_buf()
  for _, buf in ipairs(api.nvim_list_bufs()) do
    if cur_buf ~= buf then
      pcall(vim.cmd, 'bd ' .. buf)
    end
  end
end


function M.reload(name, children)
  children = children or false
  package.loaded[name] = nil
  if children then
    for pkg_name, _ in pairs(package.loaded) do
      if vim.startswith(pkg_name, name) then
        package.loaded[pkg_name] = nil
      end
    end
  end
  return require(name)
end

M.modules = setmetatable({}, {
  __index = function(_, k)
    return M.reload(k)
  end
})


function M.activate_reload(name, children)
  name = name or vim.fn.fnamemodify(api.nvim_buf_get_name(0), ':t:r')
  children = children or false
  vim.cmd('augroup lua-debug')
  vim.cmd('au!')
  vim.cmd(string.format("autocmd BufWritePost <buffer> lua U.reload('%s', %s)", name, children))
  vim.cmd('augroup end')
end


function M.setup()
  require('jdtls').jol_path = os.getenv('HOME') .. '/apps/jol.jar'
  require('jdtls.ui').pick_one_async = require('fzy').pick_one
  require('dap.ui').pick_one = require('fzy').pick_one
  require('me.lsp.conf').setup()
  require('me.dap.conf').setup()
  require('lint').linters_by_ft = {
    markdown = {'vale'},
    rst = {'vale'},
  }

  U = M
  P = function(...)
    print(unpack(vim.tbl_map(vim.inspect, {...})))
  end
  PL = function(...)
    local log_date_format = "%FT%H:%M:%S%z"
    local fp = io.open('/tmp/nvim-debug.log', 'a+')
    local line = table.concat(vim.tbl_map(vim.inspect, {...}), ', ')
    fp:write('[' .. os.date(log_date_format) .. '] ' .. line .. '\n')
    fp:flush()
    fp:close()
  end
end


function M.reload_dap()
  require('dap.repl').close()
  U.reload('dap', true)
  U.reload('me.dap.conf')
  require('me.dap.conf').setup()
  U.reload('jdtls.dap').setup_dap({hotcodereplace = 'auto'})
  require('dap.ui').pick_one = require('fzy').pick_one
end


function M.format_uri(uri)
  if vim.startswith(uri, 'jdt://') then
    local package = uri:match('contents/[%a%d.-]+/([%a%d.-]+)') or ''
    local class = uri:match('contents/[%a%d.-]+/[%a%d.-]+/([%a%d$]+).class') or ''
    return string.format('%s::%s', package, class)
  else
    return vim.fn.fnamemodify(vim.uri_to_fname(uri), ':.')
  end
end


-- quickfixtextfunc to be used with nvim-jdtls:
-- Turns entries like `jdt://contents/java.xml/[...]/ListDatatypeValidator.class` into `package.name::ClassName: [Class] ListDatatypeValidator`
function M.quickfixtext(opts)
  if opts.quickfix == 0 then
    return nil
  end
  local qflist = vim.fn.getqflist({ id = opts.id, items = 0, title = 0, context = 0 })
  if qflist.title ~= 'Language Server' then
    return nil
  end
  local result = {}
  for i, item in pairs(qflist.items) do
    if i >= opts.start_idx and i <= opts.end_idx then
      table.insert(result, string.format('%s|%d %d|%s',
        M.format_uri(vim.uri_from_bufnr(item.bufnr)),
        item.lnum,
        item.col,
        item.text
      ))
    end
  end
  return result
end


return M
