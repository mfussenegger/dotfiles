local M = {}
local api = vim.api

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


local function reload(name, children)
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


function M.setup()
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
  U = {}
  U.reload = reload
  U.module = setmetatable({}, {
    __index = function(_, k)
      return reload(k)
    end
  })
  U.activate_reload = function(name, children)
    name = name or vim.fn.fnamemodify(api.nvim_buf_get_name(0), ':t:r')
    children = children or false
    vim.cmd('augroup lua-debug')
    vim.cmd('au!')
    vim.cmd(string.format("autocmd BufWritePost <buffer> lua U.reload('%s', %s)", name, children))
    vim.cmd('augroup end')
  end
end


return M
