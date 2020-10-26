
local uv = vim.loop
local is_windows = uv.os_uname().version:match("Windows")
local path_sep = is_windows and "\\" or "/"

local M = {}
local is_fs_root
if is_windows then
    is_fs_root = function(path)
        return path:match("^%a:$")
    end
else
    is_fs_root = function(path)
        return path == "/"
    end
end


function M.reload(name)
  package.loaded[name] = nil
  return require(name)
end

M.RE = setmetatable({}, {
  __index = function(_, k)
    return M.reload(k)
  end
})

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


local function path_join(...)
    local result = table.concat(vim.tbl_flatten {...}, path_sep):gsub(path_sep.."+", path_sep)
    return result
end

function M.root_pattern(bufnr, patterns)
  local bufname = vim.api.nvim_buf_get_name(bufnr)
  local dirname = vim.fn.fnamemodify(bufname, ':p:h')
  while not is_fs_root(dirname) do
    for _, marker in ipairs(patterns) do
      if uv.fs_stat(path_join(dirname, marker)) then
        return dirname
      end
    end
    dirname = vim.fn.fnamemodify(dirname, ':h')
  end
end


function M.init_hl(ft)
  local parser = vim.treesitter.get_parser(0, ft)
  local query = require('vim.treesitter.query').get_query(ft, 'highlights')
  if query then
    vim.treesitter.highlighter.new(parser, query)
  end
end


--- Like :only but delete other buffers
function M.only()
  local cur_buf = vim.api.nvim_get_current_buf()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if cur_buf ~= buf then
      pcall(vim.cmd, 'bd ' .. buf)
    end
  end
end
return M
