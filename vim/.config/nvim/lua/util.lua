-- from https://github.com/neovim/nvim-lsp/blob/master/lua/nvim_lsp/util.lua

local is_windows = vim.loop.os_uname().version:match("Windows")
local path_sep = is_windows and "\\" or "/"
local strip_dir_pat = path_sep.."([^"..path_sep.."]+)$"
local strip_sep_pat = path_sep.."$"

local is_fs_root

local M = {}

function M.reload(name)
  package.loaded[name] = nil
  return require(name)
end

M.RE = setmetatable({}, {
  __index = function(_, k)
    return M.reload(k)
  end
})

if is_windows then
    is_fs_root = function(path)
        return path:match("^%a:$")
    end
else
    is_fs_root = function(path)
        return path == "/"
    end
end


function M.jump_to_buf(buf, range)
    vim.api.nvim_set_current_buf(buf)
    local row = range.start.line
    local col = range.start.character
    local line = vim.api.nvim_buf_get_lines(0, row, row + 1, true)[1]
    col = vim.str_byteindex(line, col)
    vim.api.nvim_win_set_cursor(0, { row + 1, col })
end


function M.err_message(...)
  vim.api.nvim_err_writeln(table.concat(vim.tbl_flatten{...}))
  vim.api.nvim_command("redraw")
end


function M.exists(filename)
    local stat = vim.loop.fs_stat(filename)
    return stat and stat.type or false
end

local function dirname(path)
    if not path then return end
    local result = path:gsub(strip_sep_pat, ""):gsub(strip_dir_pat, "")
    if #result == 0 then
        return "/"
    end
    return result
end

local function path_join(...)
    local result = table.concat(vim.tbl_flatten {...}, path_sep):gsub(path_sep.."+", path_sep)
    return result
end

local function iterate_parents(path)
    path = vim.loop.fs_realpath(path)
    local function it(_, v)
        if not v then return end
        if is_fs_root(v) then return end
        return dirname(v), path
    end
    return it, path, path
end

local function search_ancestors(startpath, fn)
  if fn(startpath) then return startpath end
  for path in iterate_parents(startpath) do
    if fn(path) then return path end
  end
end


function M.root_pattern(bufnr, patterns)
  local function matcher(path)
    for _, pattern in ipairs(patterns) do
      if M.exists(path_join(path, pattern)) then
        return path
      end
    end
  end

  local filepath = vim.api.nvim_buf_get_name(bufnr)
  local path = dirname(filepath)
  return search_ancestors(path, matcher)
end


function M.init_hl(ft)
  local parser = vim.treesitter.get_parser(0, ft)
  local query = require('vim.treesitter.query').get_query(ft, 'highlights')
  if query then
    vim.treesitter.highlighter.new(parser, query)
  end
end


return M
