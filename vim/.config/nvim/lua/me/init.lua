local M = {}
local api = vim.api

function M.statusline()
  local parts = {
    [[%<» %{luaeval("require'me'.file_or_lsp_status()")} %m%r%=]],
    "%#warningmsg#",
    "%{&paste?'[paste] ':''}",
    "%*",

    "%#warningmsg#",
    "%{&ff!='unix'?'['.&ff.'] ':''}",
    "%*",

    "%#warningmsg#",
    "%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.'] ':''}",
    "%*",
    [[%{luaeval("require'me'.dap_status()")}]],
    [[%{luaeval("require'me'.diagnostic_status()")}]],
  }
  return table.concat(parts, '')
end


function M.diagnostic_status()
  local num_errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
  if num_errors > 0 then
    return ' 💀 ' .. num_errors .. ' '
  end
  local num_warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
  if num_warnings > 0 then
    return ' 💩' .. num_warnings .. ' '
  end
  return ''
end


function M.dap_status()
  local ok, dap = pcall(require, 'dap')
  if not ok then
    return ''
  end
  local status = dap.status()
  if status ~= '' then
    return status .. ' | '
  end
  return ''
end


function M.file_or_lsp_status()
  local messages = vim.lsp.util.get_progress_messages()
  local mode = api.nvim_get_mode().mode
  if mode ~= 'n' or vim.tbl_isempty(messages) then
    return M.format_uri(vim.uri_from_bufnr(api.nvim_get_current_buf()))
  end
  local percentage
  local result = {}
  for _, msg in pairs(messages) do
    if msg.message then
      table.insert(result, msg.title .. ': ' .. msg.message)
    else
      table.insert(result, msg.title)
    end
    if msg.percentage then
      percentage = math.max(percentage or 0, msg.percentage)
    end
  end
  if percentage then
    return string.format('%03d: %s', percentage, table.concat(result, ', '))
  else
    return table.concat(result, ', ')
  end
end

local function get_parser(bufnr)
  local ok, parser = pcall(vim.treesitter.get_parser, bufnr)
  local err
  if ok then
    return parser
  else
    err = parser
  end
  if string.find(vim.bo.filetype, '%.') then
    for ft in string.gmatch(vim.bo.filetype, '([^.]+)') do
      ok, parser = pcall(vim.treesitter.get_parser, bufnr, ft)
      if ok then
        return parser
      end
    end
  end
  error(err)
end


function M.init_hl()
  local bufnr = api.nvim_get_current_buf()
  local ok, parser = pcall(get_parser, bufnr)
  if not ok then
    return
  end
  vim.treesitter.start(bufnr, parser._lang)
end


--- Like :only but delete other buffers
function M.only()
  local cur_buf = api.nvim_get_current_buf()
  for _, buf in ipairs(api.nvim_list_bufs()) do
    if cur_buf ~= buf then
      pcall(vim.cmd, 'bd ' .. buf)
    end
  end
end


function M.reload(name, children)
  children = children or false
  package.loaded[name] = nil
  if children then
    for pkg_name, _ in pairs(package.loaded) do
      if vim.startswith(pkg_name, name) then
        package.loaded[pkg_name] = nil
      end
    end
  end
  return require(name)
end

M.modules = setmetatable({}, {
  __index = function(_, k)
    return M.reload(k)
  end
})


function M.paste(args)
  local cmd = "wl-paste --no-newline"
  local path = "/tmp/tmp.paste"
  if args then
    cmd = cmd .. " " .. args
  end
  return function()
    local types = vim.fn.system("wl-paste -l")
    if types:find('text/plain') or types:find('TEXT') or types:find('STRING') then
      return { vim.split(vim.fn.system(cmd), "\n"), "" }
    else
      vim.fn.system(cmd .. " > " .. path)
      local filetype = vim.trim(vim.fn.system("xdg-mime query filetype " .. path))
      local hash = vim.split(vim.trim(vim.fn.system("sha256sum " .. path)), " ")[1]
      local ext = vim.split(filetype, "/")[2]
      local filename = hash .. "." .. ext
      vim.fn.system("scp /tmp/tmp.paste blog@zignar.net:/home/blog/output/assets/files/" .. filename)
      return { "https://zignar.net/assets/files/" .. filename, "" }
    end
  end
end


function M.activate_reload(name, children)
  name = name or vim.fn.fnamemodify(api.nvim_buf_get_name(0), ':t:r')
  children = children or false
  vim.cmd('augroup lua-debug')
  vim.cmd('au!')
  vim.cmd(string.format("autocmd BufWritePost <buffer> lua U.reload('%s', %s)", name, children))
  vim.cmd('augroup end')
end


function M.setup()
  require('me.snippet').setup()
  require('jdtls').jol_path = os.getenv('HOME') .. '/apps/jol.jar'
  require('hop').setup()

  vim.diagnostic.config({
    virtual_text = false,
    float = {
      source = 'always',
    },
  })
  vim.diagnostic.handlers.me = {
    show = function(_, bufnr, diagnostics)
      require('me.diagnostic').set_loclist(bufnr, diagnostics)
    end,
    hide = function()
    end
  }

  U = M
  P = function(...)
    print(unpack(vim.tbl_map(vim.inspect, {...})))
  end
  PL = function(...)
    local log_date_format = "%FT%H:%M:%S%z"
    local fp = io.open('/tmp/nvim-debug.log', 'a+')
    local line = table.concat(vim.tbl_map(vim.inspect, {...}), ', ')
    fp:write('[' .. os.date(log_date_format) .. '] ' .. line .. '\n')
    fp:flush()
    fp:close()
  end
  local debug_view = nil
  PB = function(...)
    if not debug_view then
      debug_view = require('dap.ui').new_view(
        function()
          return api.nvim_create_buf(false, true)
        end,
        function(buf)
          vim.cmd('split')
          api.nvim_win_set_buf(0, buf)
          return api.nvim_get_current_win()
        end
      )
    end
    debug_view.open()
    local text = table.concat(vim.tbl_map(vim.inspect, {...}), ', ')
    local lines = vim.split(vim.trim(text), '\n')
    vim.fn.appendbufline(debug_view.buf, '$', lines)
  end
  PT = function()
    PL(debug.traceback("Stack trace"))
    PL(debug.getinfo(1))
  end
end


function M.format_uri(uri)
  if vim.startswith(uri, 'jdt://') then
    local package = uri:match('contents/[%a%d._-]+/([%a%d._-]+)') or ''
    local class = uri:match('contents/[%a%d._-]+/[%a%d._-]+/([%a%d$]+).class') or ''
    return string.format('%s::%s', package, class)
  else
    local fname = vim.fn.fnamemodify(vim.uri_to_fname(uri), ':.')
    fname = fname:gsub('src/main/java/', 's/m/j/')
    fname = fname:gsub('src/test/java/', 's/t/j/')
    return fname
  end
end


-- quickfixtextfunc to be used with nvim-jdtls:
-- Turns entries like `jdt://contents/java.xml/[...]/ListDatatypeValidator.class` into `package.name::ClassName: [Class] ListDatatypeValidator`
function M.quickfixtext(opts)
  if opts.quickfix == 0 then
    return nil
  end
  local qflist = vim.fn.getqflist({ id = opts.id, items = 0, title = 0, context = 0 })
  local ctx = qflist.context or {}
  if not ctx.client_id and qflist.title ~= 'Language Server' and qflist.title ~= 'LSP locations' then
    return nil
  end
  local result = {}
  for i, item in pairs(qflist.items) do
    if i >= opts.start_idx and i <= opts.end_idx then
      table.insert(result, string.format('%s|%d %d|%s',
        M.format_uri(vim.uri_from_bufnr(item.bufnr)),
        item.lnum,
        item.col,
        item.text
      ))
    end
  end
  return result
end


function M.dump_hl(name, path)
  local header = string.format([[
vim.o.background = '%s'
vim.cmd.highlight 'clear'
if vim.g.syntax_on then
  vim.cmd.syntax 'reset'
end
vim.g.colors_name = '%s'

]], vim.o.background, name)

  local f = io.open(path, "w")
  assert(f, "Could not open " .. path)
  f:write(header)
  local colors = {
    [vim.g.terminal_color_0] = "black",
    [vim.g.terminal_color_1] = "red",
    [vim.g.terminal_color_2] = "green",
    [vim.g.terminal_color_3] = "yellow",
    [vim.g.terminal_color_4] = "blue",
    [vim.g.terminal_color_5] = "magenta",
    [vim.g.terminal_color_6] = "cyan",
    [vim.g.terminal_color_7] = "white",
    [vim.g.terminal_color_8] = "bright_black",
    [vim.g.terminal_color_9] = "bright_red",
    [vim.g.terminal_color_10] = "bright_green",
    [vim.g.terminal_color_11] = "bright_yellow",
    [vim.g.terminal_color_12] = "bright_blue",
    [vim.g.terminal_color_13] = "bright_magenta",
    [vim.g.terminal_color_14] = "bright_cyan",
    [vim.g.terminal_color_15] = "bright_white",
  }
  f:write("local black = '" .. vim.g.terminal_color_0 .. "'\n")
  f:write("local red = '" .. vim.g.terminal_color_1 .. "'\n")
  f:write("local green = '" .. vim.g.terminal_color_2 .. "'\n")
  f:write("local yellow = '" .. vim.g.terminal_color_3 .. "'\n")
  f:write("local blue = '" .. vim.g.terminal_color_4 .. "'\n")
  f:write("local magenta = '" .. vim.g.terminal_color_5 .. "'\n")
  f:write("local cyan = '" .. vim.g.terminal_color_6 .. "'\n")
  f:write("local white = '" .. vim.g.terminal_color_7 .. "'\n")
  f:write("local bright_black = '" .. vim.g.terminal_color_8 .. "'\n")
  f:write("local bright_red = '" .. vim.g.terminal_color_9 .. "'\n")
  f:write("local bright_green = '" .. vim.g.terminal_color_10 .. "'\n")
  f:write("local bright_yellow = '" .. vim.g.terminal_color_11 .. "'\n")
  f:write("local bright_blue = '" .. vim.g.terminal_color_12 .. "'\n")
  f:write("local bright_magenta = '" .. vim.g.terminal_color_13 .. "'\n")
  f:write("local bright_cyan = '" .. vim.g.terminal_color_14 .. "'\n")
  f:write("local bright_white = '" .. vim.g.terminal_color_15 .. "'\n")
  f:write('vim.g.terminal_color_0 = black\n')
  f:write('vim.g.terminal_color_1 = red\n')
  f:write('vim.g.terminal_color_2 = green\n')
  f:write('vim.g.terminal_color_3 = yellow\n')
  f:write('vim.g.terminal_color_4 = blue\n')
  f:write('vim.g.terminal_color_5 = magenta\n')
  f:write('vim.g.terminal_color_6 = cyan\n')
  f:write('vim.g.terminal_color_7 = white\n')
  f:write('vim.g.terminal_color_8 = bright_black\n')
  f:write('vim.g.terminal_color_9 = bright_red\n')
  f:write('vim.g.terminal_color_10 = bright_green\n')
  f:write('vim.g.terminal_color_11 = bright_yellow\n')
  f:write('vim.g.terminal_color_12 = bright_blue\n')
  f:write('vim.g.terminal_color_13 = bright_magenta\n')
  f:write('vim.g.terminal_color_14 = bright_cyan\n')
  f:write('vim.g.terminal_color_15 = bright_white\n')
  f:write('local theme = {\n')
  for hl_name, hl in pairs(vim.api.nvim__get_hl_defs(0)) do
    if not vim.startswith(hl_name, '@') then
      f:write('  ')
      f:write(hl_name)
      f:write(' = { ')
      for k, v in pairs(hl) do
        if type(k) ~= "boolean" then
          if k == "foreground" then
            f:write("fg")
          elseif k == "background" then
            f:write("bg")
          else
            f:write(k)
          end
          f:write(' = ')
          if type(v) == "string" then
            f:write("'")
            f:write(v)
            f:write("'")
          elseif k ~= "blend" and type(v) == "number" then
            local color_code = string.format("#%06x", v)
            local color_name = colors[color_code]
            if color_name then
              f:write(color_name)
            else
              f:write("'")
              f:write(color_code)
              f:write("'")
            end
          else
            f:write(tostring(v))
          end
          if next(hl, k) then
            f:write(', ')
          end
        end
      end
      f:write(' },\n')
    end
  end
  f:write('}\n')
  f:write([[
for k, v in pairs(theme) do
  vim.api.nvim_set_hl(0, k, v)
end
]])
  f:close()
end


return M
