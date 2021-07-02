local fzy = require('fzy')
local api = vim.api


fzy.format_bufname = function(bufnr)
  local uri = vim.uri_from_bufnr(bufnr)
  return require('me').format_uri(uri)
end


function fzy.emoji()
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

return fzy
