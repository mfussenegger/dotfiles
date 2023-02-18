local api = vim.api
local HOME = os.getenv('HOME')
local M = {}
local log_level = 'INFO'

local function add_tagfunc(widget)
  local orig_new_buf = widget.new_buf
  widget.new_buf = function(...)
    local bufnr = orig_new_buf(...)
    api.nvim_buf_set_option(bufnr, "tagfunc", "v:lua.require'me.lsp.ext'.symbol_tagfunc")
    return bufnr
  end
end


local function reload()
  local m = require('me')
  require('dap.repl').close()
  m.reload('dap', true)
  m.reload('me.dap').setup()
  m.reload('jdtls.dap').setup_dap({hotcodereplace = 'auto'})
  vim.cmd('set ft=' .. vim.bo.filetype)
  require('dap').set_log_level(log_level)
end


function M.setup()
  local dap = require('dap')

  local orig_set_log_level = dap.set_log_level
  function dap.set_log_level(level)
    orig_set_log_level(level)
    log_level = level
  end

  local widgets = require('dap.ui.widgets')
  add_tagfunc(widgets.expression)
  add_tagfunc(widgets.scopes)
  local keymap = vim.keymap
  local function set(mode, lhs, rhs)
    keymap.set(mode, lhs, rhs, { silent = true })
  end
  set({'n', 't'}, '<F3>',  dap.terminate)
  set({'n', 't'}, '<F5>',  dap.continue)
  set('n', '<leader>b', dap.toggle_breakpoint)
  set('n', '<leader>B', function()
    dap.toggle_breakpoint(vim.fn.input('Breakpoint Condition: '), nil, nil, true)
  end)
  set('n', '<leader>lp', function()
    dap.toggle_breakpoint(nil, nil, vim.fn.input('Log point message: '), true)
  end)
  set('n', '<leader>dr', function() dap.repl.toggle({height=15}) end)
  set('n', '<leader>dR', dap.restart_frame)
  set('n', '<leader>dl', function()
    if vim.bo.modified and vim.bo.buftype == '' then
      vim.cmd.w()
    end
    dap.run_last()
  end)
  set('n', '<leader>dj', dap.down)
  set('n', '<leader>dk', dap.up)
  set('n', '<leader>dc', dap.run_to_cursor)
  set('n', '<leader>dg', dap.goto_)
  set('n', '<leader>dS', function() widgets.centered_float(widgets.frames) end)
  set('n', '<leader>dt', function() widgets.centered_float(widgets.threads) end)
  set('n', '<leader>ds', function() widgets.centered_float(widgets.scopes) end)
  set({'n', 'v'}, '<leader>dh', widgets.hover)
  set({'n', 'v'}, '<leader>dp', widgets.preview)

  dap.listeners.after.event_initialized['me.dap'] = function()
    set("n", "<up>", dap.restart_frame)
    set("n", "<down>", dap.step_over)
    set("n", "<left>", dap.step_out)
    set("n", "<right>", dap.step_into)
    vim.o.signcolumn = 'yes:1'
  end
  local after_session = function()
    if not next(dap.sessions()) then
      pcall(keymap.del, "n", "<up>")
      pcall(keymap.del, "n", "<down>")
      pcall(keymap.del, "n", "<left>")
      pcall(keymap.del, "n", "<right>")
      vim.o.signcolumn = 'auto'
    end
  end
  dap.listeners.after.event_terminated['me.dap'] = after_session
  dap.listeners.after.disconnect['me.dap'] = after_session

  local sidebar = widgets.sidebar(widgets.scopes)
  api.nvim_create_user_command('DapSidebar', sidebar.toggle, { nargs = 0 })
  api.nvim_create_user_command('DapReload', reload, { nargs = 0 })
  api.nvim_create_user_command('DapBreakpoints', function() dap.list_breakpoints(true) end, { nargs = 0 })

  local sessions_bar = widgets.sidebar(widgets.sessions, {}, '5 sp')
  api.nvim_create_user_command("DapSessions", sessions_bar.toggle, { nargs = 0 })


  dap.defaults.fallback.switchbuf = 'usetab,uselast'
  dap.defaults.fallback.terminal_win_cmd = 'tabnew'
  dap.defaults.python.terminal_win_cmd = 'belowright new'
  dap.defaults.fallback.external_terminal = {
    command = '/usr/bin/alacritty';
    args = {'--hold', '-e'};
  }
  dap.adapters.cppdbg = {
    id = 'cppdbg',
    type = 'executable',
    command = HOME .. '/apps/cpptools/extension/debugAdapters/bin/OpenDebugAD7',
  }
  dap.adapters.codelldb = {
    type = 'server',
    port = "${port}",
    executable = {
      command = HOME .. '/apps/codelldb/extension/adapter/codelldb',
      args = {"--port", "${port}"},
    }
  }
  dap.adapters.lldb = {
    type = 'executable',
    command = '/usr/bin/lldb-vscode',
    name = "lldb"
  }
  -- Dont forget, attach needs permission:
  --  echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
  local configs = {
    {
      name = "cppdbg: Launch",
      type = "cppdbg",
      request = "launch",
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
      externalConsole = true,
      args = {},
    },
    {
      name = "cppdbg: Attach",
      type = "cppdbg",
      request = "Attach",
      processId = function()
        return tonumber(vim.fn.input("Pid: "))
      end,
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
      args = {},
    },
    {
      name = "codelldb: Launch",
      type = "codelldb",
      request = "launch",
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
      args = {},
    },
    {
      name = "codelldb: Launch external",
      type = "codelldb",
      request = "launch",
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
      args = {},
      terminal = 'external',
    },
    {
      name = "codelldb: Attach (select process)",
      type = 'codelldb',
      request = 'attach',
      pid = require('dap.utils').pick_process,
      args = {},
    },
    {
      name = "codelldb: Attach (input pid)",
      type = 'codelldb',
      request = 'attach',
      pid = function()
        return tonumber(vim.fn. input('pid: '))
      end,
      args = {},
    },
    {
      name = "lldb: Launch (integratedTerminal)",
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
      name = "lldb: Launch (console)",
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
      name = "lldb: Attach to process",
      type = 'lldb',
      request = 'attach',
      pid = require('dap.utils').pick_process,
      args = {},
    },
    setmetatable(
      {
        name = "Neovim",
        type = "cppdbg",
        request = "launch",
        program = os.getenv("HOME") .. "/dev/neovim/neovim/build/bin/nvim",
        cwd = os.getenv("HOME") .. "/dev/neovim/neovim/",
        externalConsole = true,
        args = {
           "--clean",
           "--cmd",
           "call getchar()",
        }
      },
      {
        __call = function(config)
          local key = "me.neovim"
          dap.listeners.after.initialize[key] = function(session)
            session.on_close[key] = function()
              for _, handler in pairs(dap.listeners.after) do
                handler['me.neovim'] = nil
              end
            end
          end
          dap.listeners.after.event_process[key] = function(_, body)
            dap.listeners.after.initialize[key] = nil
            local ppid = body.systemProcessId
            vim.wait(1000, function()
              return tonumber(vim.fn.system("ps -o pid= --ppid " .. tostring(ppid))) ~= nil
            end)
            local pid = tonumber(vim.fn.system("ps -o pid= --ppid " .. tostring(ppid)))
            if pid then
              dap.run({
                name = "Neovim embedded",
                type = "cppdbg",
                request = "attach",
                program = os.getenv("HOME") .. "/dev/neovim/neovim/build/bin/nvim",
                processId = pid,
                cwd = os.getenv("HOME") .. "/dev/neovim/neovim/",
                externalConsole = false,
              })
            end
          end
          return config
        end
      }
    ),
  }
  dap.configurations.c = configs
  dap.configurations.rust = configs
  dap.configurations.cpp = configs
  require('dap.ext.vscode').type_to_filetypes = {
    lldb = {'rust', 'c', 'cpp'},
  }
  pcall(require('dap.ext.vscode').load_launchjs)
end


return M
