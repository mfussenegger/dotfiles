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
  if vim.snippet then
    vim.snippet.stop()
  end
  return '<ESC>'
end, { expr = true })


local function try_jump(direction, key)
  if vim.snippet and vim.snippet.active({direction = direction}) then
    return string.format("<cmd>lua vim.snippet.jump(%d)<cr>", direction)
  end
  return key
end
keymap.set({"i", "s"}, "<Tab>", function() return try_jump(1, "<Tab>") end, { expr = true })
keymap.set({"i", "s"}, "<S-Tab>", function() return try_jump(-1, "<S-Tab>") end, { expr = true })


keymap.set('n', 'gs', [[:let @/='\<'.expand('<cword>').'\>'<CR>cgn]])
keymap.set('x', 'gs', [["sy:let @/=@s<CR>cgn]])

create_autocmd('WinEnter', { callback = function() vim.wo.cursorline = true end })
create_autocmd('WinLeave', { callback = function() vim.wo.cursorline = false end })


keymap.set('n', '<leader>q', function() require('quickfix').toggle() end, { silent = true })
keymap.set('n', '<leader>lq', function() require('quickfix').load() end, { silent = true })

local me = require('me')
me.setup()
require('me.fzy').setup()
require('me.dap').setup()
require('me.lsp').setup()



local has_osc52, osc52 = pcall(require, 'vim.ui.clipboard.osc52')
if has_osc52 and os.getenv("SSH_CLIENT") then
  vim.g.clipboard = {
    name = 'OSC 52',
    copy = {
      ['+'] = osc52.copy,
      ['*'] = osc52.copy,
    },
    paste = {
      ['+'] = osc52.paste,
      ['*'] = osc52.paste,
    },
  }
else
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
end


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
    if (byte_count > 200000) then
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
    if vim.loop.fs_stat(tmpl) then
      vim.cmd("0r " .. tmpl)
    else
      local ext = vim.fn.fnamemodify(args.file, ":e")
      tmpl = home .. "/.config/nvim/templates/" .. ext ..".tpl"
      if vim.loop.fs_stat(tmpl) then
        vim.cmd("0r " .. tmpl)
      end
    end
  end
})


vim.cmd.colorscheme("gruvbox-material")
create_autocmd("OptionSet", {
  group = api.nvim_create_augroup("colors", { clear = true }),
  nested = true,
  pattern = "background",
  callback = function()
    if vim.o.background == "light" then
      vim.cmd.colorscheme("tempus_totus")
    else
      vim.cmd.colorscheme("gruvbox-material")
    end
  end
})
