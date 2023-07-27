local api = vim.api
local create_autocmd = api.nvim_create_autocmd
local keymap = vim.keymap

vim.cmd [[
  source ~/.config/nvim/options.vim
  source ~/.config/nvim/mappings.vim
]]

vim.g.python3_host_prog = vim.fn.expand('$HOME/.virtualenvs/nvim/bin/python')
vim.g.netrw_liststyle = 3

vim.o.laststatus = 3
vim.o.scrollback = 100000
vim.o.signcolumn = "auto"
vim.o.pumheight = 20

api.nvim_create_autocmd("WinEnter", {
  callback = function()
    local win = api.nvim_get_current_win()
    local value = (
      (vim.bo.buftype == "" and package.loaded.dap and require("dap").session())
      and "yes:1"
      or "auto"
    )
    vim.wo[win].signcolumn = value
  end,
})


do
  local function feedkeys(keys, mode)
    mode = mode or "n"
    api.nvim_feedkeys(
      api.nvim_replace_termcodes(keys, true, false, true), mode, true)
  end

  keymap.set("i", "<c-l>", function()
    local info = vim.fn.complete_info({"pum_visible", "selected"})
    if info.pum_visible == 1 then
      if info.selected == -1 then
        feedkeys("<C-n>", "n")
      end
      require("lsp_compl").accept_pum()
      feedkeys("<c-y>", "n")
    else
      if next(vim.lsp.get_active_clients({ bufnr = 0 })) then
        require("lsp_compl").trigger_completion()
      else
        if vim.bo.omnifunc == "" then
          feedkeys("<C-x><C-n>", "n")
        else
          feedkeys("<C-x><C-o>", "n")
        end
      end
    end
  end)
end

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
      vim.cmd.normal("zz")
      vim.cmd.redraw()
      local ok, keynum = pcall(vim.fn.getchar)
      if not ok then
        break
      end
      local key = string.char(keynum)
      local fn
      if key == "]" then
        fn = opts.next
      elseif key == "[" then
        fn = opts.prev
      else
        break
      end
      local jump_ok, err = pcall(fn)
      if not jump_ok then
        vim.notify(err, vim.log.levels.WARN)
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
    lua = {'codespell', 'luacheck'},
    ['yaml.ansible'] = {'ansible_lint'},
    yaml = {'yamllint'},
    gitcommit = {'codespell'},
    dockerfile = {'hadolint'},
    ghaction = {"actionlint"},
  }
  create_autocmd({'BufWritePost', 'BufEnter'}, {
    group = api.nvim_create_augroup('lint', { clear = true }),
    callback = function() lint.try_lint() end,
  })
end


api.nvim_create_user_command("Del", function(args)
  local bufnr = api.nvim_get_current_buf()
  local fname = api.nvim_buf_get_name(bufnr)
  local ok, err = os.remove(fname)
  assert(args.bang or ok, err)
  api.nvim_buf_delete(bufnr, { force = args.bang })
end, { bang = true })
api.nvim_create_user_command("GV", function(args)
    local fname = api.nvim_buf_get_name(0)
    local cmd = {
      'G log --date=short --format="%cd %h%d %s (%an)"',
      string.format("-L%d,%d:%s", args.line1, args.line2, fname),
    }
    vim.cmd(table.concat(cmd, " "))
  end, { range = "%" }
)
api.nvim_create_user_command("B", function(args)
  local curbuf = api.nvim_get_current_buf()
  local altbuf = nil
  for _, buf in ipairs(api.nvim_list_bufs()) do
    if buf ~= curbuf and api.nvim_buf_is_loaded(buf) and vim.bo[buf].buftype == "" then
      altbuf = buf
      break
    end
  end
  if not altbuf then
    altbuf = api.nvim_create_buf(true, true)
  end
  for _, win in ipairs(api.nvim_list_wins()) do
    if api.nvim_win_get_buf(win) == curbuf then
      api.nvim_win_set_buf(win, altbuf)
    end
  end
  api.nvim_buf_delete(curbuf, { force = args.bang })
end, { bang = true })


vim.filetype.add({
  pattern = {
    ['.*/.github/workflows/.*%.yml'] = 'yaml.ghaction',
    ['.*/.github/workflows/.*%.yaml'] = 'yaml.ghaction',
  },
})
