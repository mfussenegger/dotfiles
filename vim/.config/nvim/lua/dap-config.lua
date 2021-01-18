local dap = require('dap')
local HOME = os.getenv('HOME')

dap.defaults.fallback.external_terminal = {
  command = '/usr/bin/alacritty';
  args = {'-e'};
}

require('dap-python').setup('~/.virtualenvs/tools/bin/python')

dap.configurations.java = {
  {
    type = 'java';
    request = 'attach';
    name = "Debug (Attach) - Remote";
    hostName = "127.0.0.1";
    port = 5005;
  },
}

dap.adapters.go = {
  type = 'executable';
  command = 'node';
  args = {HOME .. '/dev/golang/vscode-go/dist/debugAdapter.js'};
  enrich_config = function(conf, on_config)
    if not conf.dlvToolPath then
      conf.dlvToolPath = '/usr/bin/dlv'
    end
    on_config(conf)
  end;
}
dap.configurations.go = {
  {
    type = 'go';
    name = 'Debug';
    request = 'launch';
    showLog = true;
    program = "${file}";
  },
}

dap.adapters.haskell = {
  type = 'executable';
  command = 'haskell-debug-adapter';
  args = {'--hackage-version=0.0.33.0'};
}
dap.configurations.haskell = {
  {
    type = 'haskell',
    request = 'launch',
    name = 'Debug',
    workspace = '${workspaceFolder}',
    startup = "${file}",
    stopOnEntry = true,
    logFile = vim.fn.stdpath('data') .. '/haskell-dap.log',
    logLevel = 'WARNING',
    ghciEnv = vim.empty_dict(),
    ghciPrompt = "λ: ",
    ghciInitialPrompt = "λ: ",
    ghciCmd= "stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show",
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
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
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

dap.adapters.c = dap.adapters.cpp
dap.configurations.c = dap.configurations.cpp


require('dap.ext.vscode').load_launchjs()
