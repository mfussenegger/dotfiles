local dap = require('dap')
local HOME = os.getenv('HOME')
local M = {}


local function setup_widgets()
  local widgets = require('dap.ui.widgets')
  M.sidebar = widgets.sidebar(widgets.scopes)
end


function M.setup()
  setup_widgets()

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

  dap.configurations.lua = {
    {
      type = 'nlua',
      request = 'attach',
      name = "Attach to running Neovim instance",
      port = 44444,
    }
  }
  dap.adapters.nlua = function(callback, config)
    local port = config.port
    local opts = {
      args = {
        '-e', vim.v.progpath,
        '-c', string.format('lua require("osv").launch({port = %d})', port),
      },
      cwd = vim.fn.getcwd(),
      detached = true
    }
    local handle
    local pid_or_err
    handle, pid_or_err = vim.loop.spawn('alacritty', opts, function(code)
      handle:close()
      if code ~= 0 then
        print('nvim exited', code)
      end
    end)
    assert(handle, 'Could not run alacritty:' .. pid_or_err)

    -- doing a `client = new_tcp(); client:connect()` within vim.wait doesn't work
    -- because an extra client connecting confuses `osv`, so sleep a bit instead
    -- to wait until server is started
    vim.cmd('sleep')
    callback({ type = 'server', host = '127.0.0.1', port = port })
  end

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
    {
      -- If you get an "Operation not permitted" error using this, try disabling YAMA:
      --  echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
      name = "Attach to process",
      type = 'cpp',
      request = 'attach',
      pid = function()
        local output = vim.fn.system({'ps', 'a'})
        local lines = vim.split(output, '\n')
        local procs = {}
        for _, line in pairs(lines) do
          -- output format
          --    " 107021 pts/4    Ss     0:00 /bin/zsh <args>"
          local parts = vim.fn.split(vim.fn.trim(line), ' \\+')
          local pid = parts[1]
          local name = table.concat({unpack(parts, 5)}, ' ')
          if pid and pid ~= 'PID' then
            pid = tonumber(pid)
            if pid ~= vim.fn.getpid() then
              table.insert(procs, { pid = tonumber(pid), name = name })
            end
          end
        end
        local choices = {'Select process'}
        for i, proc in ipairs(procs) do
          table.insert(choices, string.format("%d: pid=%d name=%s", i, proc.pid, proc.name))
        end
        -- Would be cool to have a fancier selection, but needs to be sync :/
        -- Should nvim-dap handle coroutine results?
        local choice = vim.fn.inputlist(choices)
        if choice < 1 or choice > #procs then
          return nil
        end
        return procs[choice].pid
      end,
      args = {},
    },
  }
  dap.configurations.c = dap.configurations.cpp
  dap.configurations.rust = dap.configurations.cpp

  require('dap.ext.vscode').load_launchjs()
end

return M
