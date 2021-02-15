local M = {}

function M.reload(name)
  package.loaded[name] = nil
  for pkg_name, _ in pairs(package.loaded) do
    if vim.startswith(pkg_name, name) then
      package.loaded[pkg_name] = nil
    end
  end
  return require(name)
end

M.RE = setmetatable({}, {
  __index = function(_, k)
    return M.reload(k)
  end
})

function M.init_hl()
  local ft = vim.api.nvim_buf_get_option(0, 'filetype')
  local ok, parser = pcall(vim.treesitter.get_parser, 0, ft)
  if not ok then return end
  local get_query = require('vim.treesitter.query').get_query
  local ok, query = pcall(get_query, ft, 'highlights')
  if ok and query then
    vim.treesitter.highlighter.new(parser, query)
  end
end


--- Like :only but delete other buffers
function M.only()
  local cur_buf = vim.api.nvim_get_current_buf()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
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
        vim.api.nvim_feedkeys('a' .. item.emoji, 'n', true)
      end
    end
  )
end


return M
