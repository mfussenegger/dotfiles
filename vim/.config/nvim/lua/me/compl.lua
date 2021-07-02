local M = {}


function M.find_start(line, cursor_pos)
  local line_to_cursor = line:sub(1, cursor_pos)
  local idx = 0
  while true do
    local i = string.find(line_to_cursor, '[^a-zA-Z0-9_]', idx + 1)
    if i == nil then
      break
    else
      idx = i
    end
  end
  return idx + 1
end


return M
