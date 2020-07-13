local dap = require('dap')
dap.adapters.python = {
  type = 'executable';
  command = os.getenv('HOME') .. '/.virtualenvs/tools/bin/python';
  args = { '-m', 'debugpy.adapter' };
}
dap.configurations.python = {
  {
    type = 'python';
    request = 'launch';
    name = "Launch file";
    program = "${file}";
    -- console = "integratedTerminal"; -- requires https://github.com/neovim/neovim/pull/11839
    pythonPath = function()
      local cwd = vim.fn.getcwd()
      if vim.fn.executable(cwd .. '/venv/bin/python') then
        return cwd .. '/venv/bin/python'
      elseif vim.fn.executable(cwd .. '/.venv/bin/python') then
        return cwd .. '/.venv/bin/python'
      else
        return '/usr/bin/python'
      end
    end;
  },
}
dap.configurations.java = {
  {
    type = 'java';
    request = 'attach';
    name = "Debug (Attach) - Remote";
    hostName = "127.0.0.1";
    port = 5005;
  },
}


dap.adapters.cpp = {
  type = 'executable',
  command = 'lldb-vscode',
  env = {
    LLDB_LAUNCH_FLAG_LAUNCH_IN_TTY = "YES"
  },
  name = "lldb"
}

dap.configurations.cpp = {
  {
    name = "Launch",
    type = "cpp",
    request = "launch",
    program = function()
      return vim.fn.input('Path to executable: ',  vim.fn.getcwd() .. '/')
    end,
    cwd = '${workspaceFolder}',
    env = function()
      local variables = {}
      for k, v in pairs(vim.fn.environ()) do
        table.insert(variables, string.format("%s=%s", k, v))
      end
      return variables
    end,
    stopOnEntry = false,
    args = {}
  },
}

vim.cmd [[
    command! -complete=file -nargs=* DebugC lua require "dap-config".start_c_debugger({<f-args>}, "gdb")
]]
vim.cmd [[
    command! -complete=file -nargs=* DebugRust lua require "dap-config".start_c_debugger({<f-args>}, "gdb", "rust-gdb")
]]


local M = {}
local last_gdb_config

M.start_c_debugger = function(args, mi_mode, mi_debugger_path)
  if args and #args > 0 then
    last_gdb_config = vim.deepcopy(dap.configurations.cpp[1])
    last_gdb_config.name = args[1]
    last_gdb_config.program = table.remove(args, 1)
    last_gdb_config.args = args
    last_gdb_config.MIMode = mi_mode or "gdb"
    last_gdb_config.MIDebuggerPath = mi_debugger_path
  end
  if not last_gdb_config then
    print('No binary to debug set! Use ":DebugC <binary> <args>" or ":DebugRust <binary> <args>"')
    return
  end
  dap.run(last_gdb_config)
  dap.repl.open()
end

return M
