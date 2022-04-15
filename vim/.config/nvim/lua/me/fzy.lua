local M = {}

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
  vim.ui.select(items, opts, function(item)
    if item then
      vim.api.nvim_feedkeys('a' .. item.emoji, 'n', true)
    end
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
  set('n', '<leader>f/', actions.buf_lines, silent)
  set('n', '<leader>ff', function() fzy.execute('fd', fzy.sinks.edit_file) end, silent)
  set('n', '<leader>ft', function() fzy.try(actions.lsp_tags, actions.buf_tags) end, silent)
  set('n', '<leader>fg', function() fzy.execute('git ls-files', fzy.sinks.edit_file) end, silent)
  set('i', '<c-e>', emoji, silent)
  return fzy
end


return M
