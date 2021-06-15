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


-- Like fzy.actions.buffers but with custom jdt:// URI handling
function M.buffers()
  local bufs = vim.tbl_filter(
    function(b)
      return api.nvim_buf_is_loaded(b) and api.nvim_buf_get_option(b, 'buftype') ~= 'quickfix'
    end,
    api.nvim_list_bufs()
  )
  local format_bufname = function(b)
    local fullname = api.nvim_buf_get_name(b)
    local name
    if #fullname == 0 then
      name = '[No Name] (' .. api.nvim_buf_get_option(b, 'buftype') .. ')'
    else
      name= require('me').format_uri(vim.uri_from_bufnr(b))
    end
    local modified = api.nvim_buf_get_option(b, 'modified')
    return modified and name .. ' [+]' or name
  end
  fzy.pick_one(bufs, 'Buffers> ', format_bufname, function(b)
    if b then
      api.nvim_set_current_buf(b)
    end
  end)
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
