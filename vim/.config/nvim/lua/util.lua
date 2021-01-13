local M = {}

function M.reload(name)
  package.loaded[name] = nil
  return require(name)
end

M.RE = setmetatable({}, {
  __index = function(_, k)
    return M.reload(k)
  end
})

function M.init_hl(ft)
  local parser = vim.treesitter.get_parser(0, ft)
  local query = require('vim.treesitter.query').get_query(ft, 'highlights')
  if query then
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
