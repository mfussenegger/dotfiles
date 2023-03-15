local api = vim.api
local create_autocmd = api.nvim_create_autocmd
local keymap = vim.keymap

vim.cmd [[
  source ~/.config/nvim/options.vim
  source ~/.config/nvim/mappings.vim
  source ~/.config/nvim/plugin_options.vim
]]

vim.g.python3_host_prog = vim.fn.expand('$HOME/.virtualenvs/nvim/bin/python')
vim.o.laststatus = 3
vim.o.scrollback=100000
vim.o.signcolumn = require("dap").session() == nil and "auto" or "yes:1"

local accept_compl_or_cr = function()
  return require('lsp_compl').accept_pum() and '<c-y>' or '<CR>'
end
keymap.set('i', '<CR>', accept_compl_or_cr, { expr = true })
keymap.set({'i', 's'}, '<ESC>', function()
  require('luasnip').unlink_current()
  return '<ESC>'
end, { expr = true })

keymap.set('n', 'gs', [[:let @/='\<'.expand('<cword>').'\>'<CR>cgn]])
keymap.set('x', 'gs', [["sy:let @/=@s<CR>cgn]])


local function next_diagnostic()
  vim.diagnostic.goto_next({
    severity = require('me').diagnostic_severity(),
    float = { border = 'single' }
  })
end
local function prev_diagnostic()
  vim.diagnostic.goto_prev({
    severity = require('me').diagnostic_severity(),
    float = { border = 'single' }
  })
end


---@param opts lsp_tags.opts
local function select_methods(opts)
  return function()
    require('qwahl').lsp_tags(vim.tbl_extend("force", {
      kind = {'Constructor', 'Method', 'Function'},
    }, opts or {}))
  end
end

---@param opts lsp_tags.opts
local function select_methods_sync(opts)
  return function()
    local done = false
    opts.on_done = function()
      done = true
    end
    require('qwahl').lsp_tags(vim.tbl_extend("force", {
      kind = {'Constructor', 'Method', 'Function'},
    }, opts or {}))
    ---@diagnostic disable-next-line: redundant-return-value
    vim.wait(1000, function() return done == true end)
  end
end


local move_maps = {
  {']q', ':cnext<CR>'},
  {'[q', ':cprevious<CR>'},
  {']Q', ':cfirst<CR>'},
  {'[Q', ':clast<CR>'},
  {']l', ':lnext<CR>'},
  {'[l', ':lprevious<CR>'},
  {']L', ':lfirst<CR>'},
  {'[L', ':llast<CR>'},
  {']w', next_diagnostic},
  {'[w', prev_diagnostic},
  {']v', function() require('me.lsp').next_highlight() end},
  {'[v', function() require('me.lsp').prev_highlight() end},
  {']M', select_methods { mode = "next" }},
  {'[M', select_methods { mode = "prev" }},
}
for _, move_map in pairs(move_maps) do
  keymap.set('n', move_map[1], move_map[2])
end

---@param opts {next: function, prev:function}
local function move(opts)
  return function()
    print("Move mode: Use ] or [ to move, any other char to abort: ")
    while true do
      vim.cmd.redraw()
      local ok, keynum = pcall(vim.fn.getchar)
      if not ok then
        break
      end
      local key = string.char(keynum)
      if key == "]" then
        opts.next()
      elseif key == "[" then
        opts.prev()
      else
        break
      end
    end
    print("Move mode exited")
  end
end
keymap.set('n', '<leader>]v', move({
  next = require("me.lsp").next_highlight,
  prev = require("me.lsp").prev_highlight
}))
keymap.set("n", "<leader>]q", move({
  next = function() vim.cmd("cnext") end,
  prev = function() vim.cmd("cprev") end,
}))
keymap.set("n", "<leader>]l", move({
    next = function() vim.cmd("lnext") end,
    prev = function() vim.cmd("lprev") end,
}))
keymap.set("n", "<leader>]w", move({
  next = next_diagnostic,
  prev = prev_diagnostic
}))
keymap.set("n", "<leader>]m", move({
  next = select_methods_sync { mode = "next", pick_first = true },
  prev = select_methods_sync { mode = "prev", pick_first = true },
}))

create_autocmd('WinEnter', { callback = function() vim.wo.cursorline = true end })
create_autocmd('WinLeave', { callback = function() vim.wo.cursorline = false end })


keymap.set('n', '<leader>q', function() require('quickfix').toggle() end, { silent = true })
keymap.set('n', '<leader>lq', function() require('quickfix').load() end, { silent = true })

local me = require('me')
me.setup()
require('me.fzy').setup()
require('me.dap').setup()
require('me.lsp').setup()

vim.g.clipboard = {
  name = 'wl-link-paste',
  copy = {
    ['+'] = {'wl-copy', '--type', 'text/plain'},
    ['*'] = {'wl-copy', '--primary', '--type', 'text/plain'},
  },
  paste = {
    ['+'] = me.paste(),
    ['*'] = me.paste("--primary"),
  }
}

do
  local lint = require('lint')
  lint.linters_by_ft = {
    markdown = {'vale'},
    htmldjango = {'curlylint'},
    rst = {'vale'},
    java = {'codespell'},
    python = {'ruff'},
    lua = {'codespell', 'luacheck'},
    sh = {'shellcheck'},
    ['yaml.ansible'] = {'ansible_lint'},
    yaml = {'yamllint'},
    gitcommit = {'codespell'},
    dockerfile = {'hadolint'},
  }
  create_autocmd({'BufWritePost', 'BufEnter'}, {
    group = api.nvim_create_augroup('lint', { clear = true }),
    callback = function() lint.try_lint() end,
  })
end


api.nvim_create_user_command(
  "GV",
  function(args)
    local fname = api.nvim_buf_get_name(0)
    local cmd = {
      'G log --date=short --format="%cd %h%d %s (%an)"',
      string.format("-L%d,%d:%s", args.line1, args.line2, fname),
    }
    vim.cmd(table.concat(cmd, " "))
  end,
  { range = "%" }
)

if not pcall(require, 'editorconfig') then
  vim.cmd.packadd('editorconfig.nvim')
end

do
  local did_setup = false
  local function neotest()
    vim.cmd.packadd('neotest')
    vim.cmd.packadd('neotest-plenary')
    vim.cmd.packadd('nvim-treesitter')
    local n = require('neotest')
    if not did_setup then
      did_setup = true
      n.setup({
        adapters = {
          require('neotest-plenary'),
        },
      })
    end
    return n
  end
  keymap.set('n', 't<C-n>', function() neotest().run.run() end)
  keymap.set('n', 't<C-l>', function() neotest().run.run_last() end)
  keymap.set('n', 't<C-f>', function() neotest().run.run(vim.fn.expand('%')) end)
end
