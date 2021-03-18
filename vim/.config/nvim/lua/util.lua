local M = {}
local api = vim.api
local lint_active = {}

function M.statusline()
  local parts = {
    "%<» %f %h%m%r%=",
    "%#warningmsg#",
    "%{&paste?'[paste] ':''}",
    "%*",

    "%#warningmsg#",
    "%{&ff!='unix'?'['.&ff.'] ':''}",
    "%*",

    "%#warningmsg#",
    "%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.'] ':''}",
    "%*",
  }
  local diagnostics
  local bufnr = api.nvim_get_current_buf()
  if vim.tbl_isempty(vim.lsp.buf_get_clients(0)) and not lint_active[bufnr] then
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


function M.init_hl()
  local ft = api.nvim_buf_get_option(0, 'filetype')
  local ok, parser = pcall(vim.treesitter.get_parser, 0, ft)
  if not ok then return end
  local get_query = require('vim.treesitter.query').get_query
  local query
  ok, query = pcall(get_query, ft, 'highlights')
  if ok and query then
    vim.treesitter.highlighter.new(parser, query)
  end
function M.enable_lint()
  local bufnr = api.nvim_get_current_buf()
  lint_active[bufnr] = true
  api.nvim_buf_attach(bufnr, false, {
    on_detach = function(b)
      lint_active[b] = nil
    end,
  })
  vim.cmd("augroup lint")
  vim.cmd("au!")
  vim.cmd(string.format("au BufWritePost <buffer=%d> lua require'lint'.try_lint()", bufnr))
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


function M.emoji()
  local lines = {}
  for line in io.lines(os.getenv('HOME') .. '/.config/dm/emoji.json') do
    table.insert(lines, line)
  end
  local items = vim.fn.json_decode(table.concat(lines, '\n'))
  require('fzy').pick_one(
    items,
    'Emoji> ',
    function(item) return  item.emoji .. ' ' .. item.description end,
    function(item)
      if item then
        api.nvim_feedkeys('a' .. item.emoji, 'n', true)
      end
    end
  )
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


return M
