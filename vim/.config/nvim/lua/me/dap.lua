local api = vim.api
local dap = require('dap')
local HOME = os.getenv('HOME')


local M = setmetatable({}, {
  __index = function(tbl, key)
    if key == 'widgets' then
      local val = require('dap.ui.widgets')
      rawset(tbl, key, val)
      return val
    end
    return dap[key]
  end,
})


local function add_tagfunc(widget)
  local orig_new_buf = widget.new_buf
  widget.new_buf = function(...)
    local bufnr = orig_new_buf(...)
    api.nvim_buf_set_option(bufnr, "tagfunc", "v:lua.require'me.lsp.ext'.symbol_tagfunc")
    return bufnr
  end
end


local function setup_widgets()
  local widgets = require('dap.ui.widgets')
  M.sidebar = widgets.sidebar(widgets.scopes)
  add_tagfunc(widgets.expression)
  add_tagfunc(widgets.scopes)
end


function M.setup()
  setup_widgets()

  dap.defaults.fallback.terminal_win_cmd = 'tabnew'
  dap.defaults.fallback.external_terminal = {
    command = '/usr/bin/alacritty';
    args = {'-e'};
  }
  require('dap-python').setup('~/.virtualenvs/tools/bin/python')
  table.insert(dap.configurations.python, {
    type = 'python',
    request = 'launch',
    program = '${workspaceFolder}/${file}',
    console = 'integratedTerminal',
    name = 'Launch file with autoReload',
    autoReload = {
      enable = true,
    }
  })

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

  dap.adapters.node2 = {
    type = 'executable',
    command = 'node',
    args = {os.getenv('HOME') .. '/dev/microsoft/vscode-node-debug2/out/src/nodeDebug.js'},
  }
  dap.configurations.javascript = {
    {
      type = 'node2',
      request = 'launch',
      program = '${workspaceFolder}/${file}',
      cwd = '/tmp/',
      sourceMaps = true,
      protocol = 'inspector',
      console = 'integratedTerminal',
    },
  }

  dap.adapters.netcoredbg = {
    type = 'executable',
    command = HOME .. '/.local/dotnet/netcoredbg/netcoredbg',
    args = {'--interpreter=vscode'}
  }
  dap.configurations.cs = {
    {
      type = "netcoredbg",
      name = "launch - netcoredbg",
      request = "launch",
      program = function()
          return vim.fn.input('Path to dll', vim.fn.getcwd() .. '/bin/Debug/', 'file')
      end,
      stopAtEntry = true,
    },
  }


  dap.adapters.cppdbg = {
    type = 'executable',
    command = HOME .. '/apps/cpptools/extension/debugAdapters/OpenDebugAD7',
    id = 'cppdbg',
  }
  dap.adapters.codelldb = function(on_adapter)
    local stdout = vim.loop.new_pipe(false)
    local stderr = vim.loop.new_pipe(false)
    local cmd = HOME ..  '/apps/codelldb/extension/adapter/codelldb'
    local handle, pid_or_err
    local opts = {
      stdio = {nil, stdout, stderr},
      detached = true,
    }
    handle, pid_or_err = vim.loop.spawn(cmd, opts, function(code)
      stdout:close()
      stderr:close()
      handle:close()
      if code ~= 0 then
        print("codelldb exited with code", code)
      end
    end)
    assert(handle, "Error running codelldb: " .. tostring(pid_or_err))
    stdout:read_start(function(err, chunk)
      assert(not err, err)
      if chunk then
        local port = chunk:match('Listening on port (%d+)')
        if port then
          vim.schedule(function()
            on_adapter({
              type = 'server',
              host = '127.0.0.1',
              port = port
            })
          end)
        else
          vim.schedule(function()
            require("dap.repl").append(chunk)
          end)
        end
      end
    end)
    stderr:read_start(function(err, chunk)
      assert(not err, err)
      if chunk then
        vim.schedule(function()
          require("dap.repl").append(chunk)
        end)
      end
    end)
  end
  dap.adapters.lldb = {
    type = 'executable',
    command = '/usr/bin/lldb-vscode',
    name = "lldb"
  }
  dap.configurations.cpp = {
    {
      name = 'Attach to gdbserver :1234',
      type = 'cppdbg',
      request = 'launch',
      MIMode = 'gdb',
      miDebuggerServerAddress = 'localhost:1234',
      miDebuggerPath = '/usr/bin/gdb',
      cwd = '${workspaceFolder}',
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
    },
    {
      name = "Launch via cppdgb",
      type = "cppdbg",
      request = "launch",
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
      args = {},
    },
    {
      name = "Launch via codelldb",
      type = "codelldb",
      request = "launch",
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
      args = {},
    },
    {
      name = "Launch (lldb via integrated terminal)",
      type = "lldb",
      request = "launch",
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
      stopOnEntry = false,
      args = {},
      runInTerminal = true,
    },
    {
      name = "Launch",
      type = "lldb",
      request = "launch",
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
      stopOnEntry = false,
      args = {},
      runInTerminal = false,
    },
    {
      -- If you get an "Operation not permitted" error using this, try disabling YAMA:
      --  echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
      name = "Attach to process",
      type = 'lldb',
      request = 'attach',
      pid = require('dap.utils').pick_process,
      args = {},
    },
  }
  dap.configurations.c = dap.configurations.cpp
  dap.configurations.rust = dap.configurations.cpp

  require('dap.ext.vscode').load_launchjs()
end

M.setup()
return M
