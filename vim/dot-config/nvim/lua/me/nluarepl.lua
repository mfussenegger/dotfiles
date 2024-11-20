local json = vim.json
local Client = {}
local client_mt = {__index = Client}
local rpc = require("dap.rpc")

function Client:handle_input(body)
  local request = json.decode(body)
  local handler = self[request.command]
  if handler then
    handler(self, request)
  else
    print('no handler for ' .. request.command)
  end
end


---@param request dap.Request
---@param message string
function Client:send_err_response(request, message, error)
  self.seq = request.seq + 1
  local payload = {
    seq = self.seq,
    type = 'response',
    command = request.command,
    success = false,
    request_seq = request.seq,
    message = message,
    body = {
      error = error,
    },
  }
  if self.socket then
    self.socket:write(rpc.msg_with_content_length(json.encode(payload)))
  end
end


---@param request dap.Request
---@param body any
function Client:send_response(request, body)
  self.seq = request.seq + 1
  local payload = {
    seq = self.seq,
    type = 'response',
    command = request.command,
    success = true,
    request_seq = request.seq,
    body = body,
  }
  if self.socket then
    self.socket:write(rpc.msg_with_content_length(json.encode(payload)))
  end
end


---@param event string
---@param body any
function Client:send_event(event, body)
  self.seq = self.seq + 1
  local payload = {
    seq = self.seq,
    type = 'event',
    event = event,
    body = body,
  }
  self.socket:write(rpc.msg_with_content_length(json.encode(payload)))
end


---@param command string
---@param arguments any
function Client:send_request(command, arguments)
  self.seq = self.seq + 1
  local payload = {
    seq = self.seq,
    type = "request",
    command = command,
    arguments = arguments,
  }
  self.socket:write(rpc.msg_with_content_length(json.encode(payload)))
end


function Client:initialize(request)
  self:send_response(request, {})
  self:send_event("initialized", {})
end


function Client:disconnect(request)
  self:send_event("terminated", {})
  self:send_response(request, {})
end


function Client:terminate(request)
  self:send_event("terminated", {})
  self:send_response(request, {})
end

function Client:launch(request)
  self:send_response(request, {})
end


---@param key any
---@param value any
---@return dap.Variable
function Client:_to_variable(key, value)
  local result = {
    name = tostring(key),
    value = tostring(value),
    type = type(value)
  }
  if type(value) == "table" then
    result.value = result.value .. " size=" .. vim.tbl_count(value)
    local variables = {}
    for k, v in pairs(value) do
      table.insert(variables, self:_to_variable(k, v))
    end
    local varref = self.varref + 1
    self.varref = varref
    self.vars[varref] = variables
    result.variablesReference = varref
  else
    result.variablesReference = 0
  end
  return result
end


---@param request dap.Request
function Client:evaluate(request)
  ---@type dap.EvaluateArguments
  local args = request.arguments

  local expression = args.expression
  local parser = vim.treesitter.get_string_parser(expression, "lua")
  local trees = parser:parse()
  local root = trees[1]:root() -- root is likely chunk
  local child = root:child(root:child_count() - 1)
  if child and child:type() ~= "return_statement" then
    local slnum, scol, _, _ = child:range()
    local lines = vim.split(expression, "\n", { plain = true })
    local line = lines[slnum + 1]
    lines[slnum + 1] = line:sub(1, scol) .. "return " .. line:sub(scol or 1)
    expression = table.concat(lines, "\n")
  end
  local fn, err = loadstring(expression)
  if err then
    self:send_err_response(request, tostring(err), err)
    return
  end
  assert(fn, "loadstring must return result if there is no error")

  local env = getfenv(fn)
  local newenv = {}

  function newenv.print(...)
    local line = table.concat({...})

    ---@type dap.OutputEvent
    local output = {
      category = "stdout",
      output = line .. "\n"
    }
    self:send_event("output", output)
  end

  setmetatable(newenv, {__index = env})
  setfenv(fn, newenv)

  local result = fn() or "nil"
  if type(result) == "table" then
    local tbl = result
    local variables = {}
    for k, v in pairs(tbl) do
      table.insert(variables, self:_to_variable(k, v))
    end

    local varref = self.varref + 1
    self.varref = varref
    self.vars[varref] = variables

    ---@type dap.EvaluateResponse
    local response = {
      result = tostring(tbl) .. " size=" .. tostring(vim.tbl_count(tbl)),
      variablesReference = varref,
    }
    self:send_response(request, response)
  else
    ---@type dap.EvaluateResponse
    local response = {
      result = tostring(result),
      variablesReference = 0,
    }
    self:send_response(request, response)
  end
end


---@param request dap.Request
function Client:variables(request)
  ---@type dap.VariablesArguments
  local args = request.arguments
  local variables = self.vars[args.variablesReference]
  self:send_response(request, {
    variables = variables or {}
  })
end


---@param cb fun(adapter: dap.Adapter)
local function nluarepl(cb)
  local server = assert(vim.uv.new_pipe())
  local pipe = os.tmpname()
  os.remove(pipe)
  server:bind(pipe)

  local client = {
    seq = 0,
    varref = 0,
    ---@type table<integer, dap.Variable[]>
    vars = {}
  }
  setmetatable(client, client_mt)
  server:listen(128, function(err)
    if err then
      error(vim.inspect(err))
    else
      local socket = assert(vim.uv.new_pipe())
      client.socket = socket
      server:accept(socket)
      local function on_chunk(body)
        client:handle_input(body)
      end
      local function on_eof()
        client.vars = {}
        client.varref = 0
      end
      socket:read_start(require("dap.rpc").create_read_loop(on_chunk, on_eof))
    end
  end)
  local adapter = {
    type = "pipe",
    pipe = pipe
  }
  cb(adapter)
end


return {
  nluarepl = nluarepl
}
