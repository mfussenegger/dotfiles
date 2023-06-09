#!/usr/bin/env -S v -l

package.path = package.path .. ";/usr/lib/node_modules/local-lua-debugger-vscode/debugger/?.lua"

local ffi = require("ffi")
ffi.cdef[[
  int isatty(int fd);
]]


local function execstr(str)
  local fn, err = loadstring(str)
  if fn then
    local result = fn()
    if result then
      print(result)
    end
    return result
  elseif err then
    io.stderr:write(err)
  end
end


local i = 1
while i <= #arg do
  local value = arg[i]
  if value == "-e" then
    execstr(arg[i + 1])
    i = i + 2
  else
    local fn, err = loadfile(value)
    if fn then
      local result = fn()
      if result then
        print(result)
      end
    elseif err then
      io.stderr:write(err)
    end
    i = i + 1
  end
end


if ffi.C.isatty(0) == 0 then
  for line in io.lines() do
    execstr(line)
  end
end
