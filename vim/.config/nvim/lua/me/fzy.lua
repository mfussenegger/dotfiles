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
  local fzy = require('fzy')
  fzy.format_bufname = format_bufname
  fzy.setup()
  local set = vim.keymap.set
  local silent = { silent = true }
  local actions = fzy.actions
  set('n', '<leader>fq', actions.quickfix, silent)
  set('n', '<leader>fb', actions.buffers, silent)
  set('n', '<leader>fj', actions.jumplist, silent)
  set('n', '<leader>f/', actions.buf_lines, silent)
  set('n', '<leader>ff', function() fzy.execute('fd', fzy.sinks.edit_file) end, silent)
  set('n', '<leader>ft', function() fzy.try(actions.lsp_tags, actions.buf_tags) end, silent)
  set('n', '<leader>fg', function() fzy.execute('git ls-files', fzy.sinks.edit_file) end, silent)
  set('i', '<c-e>', emoji, silent)

  local function next_methods()
    actions.lsp_tags({
      kind = {'Constructor', 'Method', 'Function'},
      mode = 'next'
    })
  end
  local function prev_methods()
    actions.lsp_tags({
      kind = {'Constructor', 'Method', 'Function'},
      mode = 'prev'
    })
  end
  set('n', ']M', next_methods, silent)
  set('n', '[M', prev_methods, silent)
  return fzy
end


return M
