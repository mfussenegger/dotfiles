local M = {}
local fzy = require('fzy')
local vfn = vim.fn
local api = vim.api


-- Like fzy.actions.quickfix but with custom jdt:// URI handling
function M.quickfix()
  vim.cmd('cclose')
  local items = vfn.getqflist()
  fzy.pick_one(
    items,
    'Quickfix> ',
    function(item)
      local uri = vim.uri_from_bufnr(item.bufnr)
      return require('me').format_uri(uri) .. ': ' .. item.text
    end,
    function(item)
      if not item then return end
      vfn.bufload(item.bufnr)
      api.nvim_win_set_buf(0, item.bufnr)
      api.nvim_win_set_cursor(0, {item.lnum, item.col - 1})
      vim.cmd('normal! zvzz')
    end
  )
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


return M
