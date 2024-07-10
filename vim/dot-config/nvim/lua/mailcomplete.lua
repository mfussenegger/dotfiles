local M = {}

function M.complete(findstart, base)
  if findstart == 1 then
    local line = vim.fn.getline('.')
    local idx = vim.fn.col('.')
    while idx > 0 do
      idx = idx - 1
      local c = line:sub(idx, idx)
      if c == ':' or c == ',' then
        return idx + 1
      end
    end
    return -2
  else
    return vim.fn.split(
      vim.fn.system('khard email -p --remove-first-line ' .. base .. ' | awk -F "\t" \'{ print $2" <"$1">" }\''),
      '\n'
    )
  end
end

return M
