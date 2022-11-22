local M = {}
local api = vim.api

local function format_bufname(bufnr)
  local uri = vim.uri_from_bufnr(bufnr)
  return require('me').format_uri(uri)
end


local function emoji()
  local lines = {}
  for line in io.lines(os.getenv('HOME') .. '/.config/dm/emoji.json') do
    table.insert(lines, line)
  end
  local items = vim.json.decode(table.concat(lines, '\n'))
  local opts = {
    prompt = 'Emoji> ',
    format_item = function(item) return item.emoji .. ' ' .. item.description end,
  }
  vim.cmd("stopinsert")
  -- Schedule ensures leaving insert mode happens before triggering ui.select
  -- This ensures terminal popup can enter insert mode
  vim.schedule(function()
    vim.ui.select(items, opts, function(item)
      if item then
        api.nvim_feedkeys('a' .. item.emoji, 'n', true)
      end
    end)
  end)
end


function M.setup()
  local q = require('qwahl')
  q.format_bufname = format_bufname
  local set = vim.keymap.set
  local silent = { silent = true }
  set('n', '<leader>fq', q.quickfix, silent)
  set('n', '<leader>fb', q.buffers, silent)
  set('n', '<leader>fj', q.tagstack, silent)
  set('n', '<leader>f/', q.buf_lines, silent)
  set('n', '<leader>ff', function()
    local fzy = require('fzy')
    fzy.execute('fd', fzy.sinks.edit_file)
  end, silent)
  set('n', '<leader>ft', function() q.try(q.lsp_tags, q.buf_tags) end, silent)
  set('n', '<leader>fg', function()
    local fzy = require('fzy')
    fzy.execute('git ls-files', fzy.sinks.edit_file)
  end, silent)
  set('n', '<leader>fm', function()
    local fzy = require('fzy')
    fzy.execute('git ls-files --modified', fzy.sinks.edit_file)
  end, silent)
  set('i', '<c-e>', emoji, silent)

  local function next_methods()
    q.lsp_tags({
      kind = {'Constructor', 'Method', 'Function'},
      mode = 'next'
    })
  end
  local function prev_methods()
    q.lsp_tags({
      kind = {'Constructor', 'Method', 'Function'},
      mode = 'prev'
    })
  end
  set('n', ']M', next_methods, silent)
  set('n', '[M', prev_methods, silent)
end


return M
