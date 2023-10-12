local api = vim.api
local create_autocmd = api.nvim_create_autocmd
local keymap = vim.keymap

vim.cmd [[
  source ~/.config/nvim/options.vim
  source ~/.config/nvim/mappings.vim
  source ~/.config/nvim/commands.lua
]]

vim.g.python3_host_prog = vim.fn.expand('$HOME/.virtualenvs/nvim/bin/python')
vim.g.netrw_liststyle = 3

vim.o.laststatus = 3
vim.o.scrollback = 100000
vim.o.signcolumn = "auto"
vim.o.pumheight = 20



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
keymap.set("n", "<leader>]c", move({
  next = function() vim.cmd.normal("]c") end,
  prev = function() vim.cmd.normal("[c") end,
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

vim.filetype.add({
  pattern = {
    ['.*/.github/workflows/.*%.yml'] = 'yaml.ghaction',
    ['.*/.github/workflows/.*%.yaml'] = 'yaml.ghaction',
  },
})


create_autocmd("FileType", {
  group = api.nvim_create_augroup("init_hl", { clear = true }),
  callback = function(args)
    local lines = api.nvim_buf_line_count(args.buf)
    local byte_count = api.nvim_buf_get_offset(args.buf, lines)
    if (byte_count > 100000) then
      vim.cmd("syn sync clear")
      vim.cmd("setlocal nowrap")
    else
      require("me").init_hl()
    end
  end
})


create_autocmd("BufNewFile", {
  group = api.nvim_create_augroup("templates", { clear = true }),
  desc = "Load template file",
  callback = function(args)
    local home = os.getenv("HOME")
    local fname = vim.fn.fnamemodify(args.file, ":t")
    local tmpl = home .. "/.config/nvim/templates/" .. fname ..".tpl"
    if vim.uv.fs_stat(tmpl) then
      vim.cmd("0r " .. tmpl)
    else
      local ext = vim.fn.fnamemodify(args.file, ":e")
      tmpl = home .. "/.config/nvim/templates/" .. ext ..".tpl"
      if vim.uv.fs_stat(tmpl) then
        vim.cmd("0r " .. tmpl)
      end
    end
  end
})
