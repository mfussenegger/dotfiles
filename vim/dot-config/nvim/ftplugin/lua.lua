local dap = require('dap')
local api = vim.api
local home = os.getenv("HOME")


local function select_cwd_and_path()
  local cwd = vim.fn.input("cwd: ", "", "dir")
  if cwd == "" then
    return vim.uv.cwd()
  end
  cwd = vim.fs.normalize(cwd)
  local fzy = require("fzy")
  local is_git = vim.uv.fs_stat(vim.fs.joinpath(cwd, ".git"))
  local ls_files = is_git and "git ls-files" or "fd"
  local shellcmd = string.format("(cd %s; %s)", vim.fn.shellescape(cwd), ls_files)
  local co = coroutine.running()
  local function on_choice(file)
    coroutine.resume(co, vim.trim(file))
  end
  fzy.execute(shellcmd, on_choice, "File> ")
  return cwd, coroutine.yield()
end


dap.adapters.osv = function(callback, conf)
  coroutine.wrap(function()
    local port = conf["port"]
    local adapter = {
      type = 'server',
      host = '127.0.0.1',
      port = port
    }
    if conf.start_neovim then
      local start_opts = conf.start_neovim
      ---@diagnostic disable-next-line: inject-field
      conf.start_neovim = nil
      if start_opts.prompt then
        start_opts.cwd, start_opts.fname = select_cwd_and_path()
      end
      local handle
      local pid_or_err
      local address = string.format('/tmp/nvim-foo-osv-%d', port)
      local opts = {
        args = {
          '-e',
          vim.v.progpath,
          '--listen',
          address,
          start_opts.fname or api.nvim_buf_get_name(0),
        },
        cwd = start_opts.cwd or vim.fn.getcwd(),
        detached = true,
      }
      handle, pid_or_err = vim.loop.spawn('/usr/bin/alacritty', opts, function(code)
        if handle then
          handle:close()
        end
        assert(code == 0, "Exit code must be 0, not " .. tostring(code))
      end)
      if not handle then
        error(pid_or_err)
      end
      vim.defer_fn(function()
        local channel = vim.fn.sockconnect("pipe", address, { rpc = true })
        assert(channel > 0)
        vim.rpcrequest(
          channel,
          "nvim_exec_lua",
          string.format("require('osv').launch({ port = %s })", port),
          {}
        )
        vim.fn.chanclose(channel)
        vim.defer_fn(vim.schedule_wrap(function() callback(adapter) end), 500)
      end, 1000)
    else
      callback(adapter)
    end
  end)()
end

dap.adapters["local-lua"] = {
  type = 'executable',
  command = 'local-lua-dbg',
  enrich_config = function(config, on_config)
    if not config["extensionPath"] then
      local c = vim.deepcopy(config)
      ---@diagnostic disable-next-line: inject-field
      c.extensionPath = "/usr/lib/node_modules/local-lua-debugger-vscode/"
      on_config(c)
    else
      on_config(config)
    end
  end
}

local function free_port()
  local tcp = vim.loop.new_tcp()
  assert(tcp)
  tcp:bind('127.0.0.1', 0)
  local port = tcp:getsockname().port
  tcp:shutdown()
  tcp:close()
  return port
end


dap.configurations.lua = {
  {
    type = "osv",
    request = "attach",
    name = "nvim:file",
    port = free_port,
    start_neovim = {}
  },
  {
    type = "osv",
    request = "attach",
    name = "nvim:prompt-cwd",
    port = free_port,
    start_neovim = {
      prompt = true
    }
  },
  {
    type = "osv",
    request = "attach",
    name = "nvim:dots",
    port = free_port,
    start_neovim = {
      cwd = home .. '/dotfiles',
      fname = 'vim/.config/nvim/init.lua',
    }
  },
  {
    type = "osv",
    request = "attach",
    name = "nvim:crate",
    port = free_port,
    start_neovim = {
      cwd = home .. '/dev/crate/crate',
      fname = 'server/src/test/java/io/crate/planner/PlannerTest.java',
    }
  },
  {
    type = "osv",
    request = "attach",
    name = "nvim:neovim",
    port = free_port,
    start_neovim = {
      cwd = home .. '/dev/neovim/neovim',
      fname = 'src/nvim/main.c',
    }
  },
  {
    type = 'osv',
    request = 'attach',
    name = 'Attach',
    port = function()
      local port = tonumber(vim.fn.input('Port: '))
      return port or dap.ABORT
    end
  },
  {
    name = 'nlua-dbg:file',
    type = 'local-lua',
    request = 'launch',
    cwd = '${workspaceFolder}',
    program = {
      lua = 'nlua',
      file = '${file}',
    },
    verbose = false,
    args = {},
  },
  {
    name = 'nbusted',
    type = 'local-lua',
    request = 'launch',
    cwd = '${workspaceFolder}',
    program = {
      command = "nbusted",
    },
    verbose = false,
    args = {
      "${file}"
    },
  },
  {
    name = 'lua-dbg:file',
    type = 'local-lua',
    request = 'launch',
    cwd = '${workspaceFolder}',
    program = {
      lua = 'lua5.1',
      file = '${file}',
    },
    args = {},
  },
}

if vim.loop.fs_stat(".stylua.toml") then
  vim.bo.formatprg = "stylua -"
end


local root_dir = vim.fs.root(0, {".git"})
local config = require("me.lsp").mk_config {
  name = "luals",
  cmd = {'lua-language-server'},
  root_dir = root_dir,
  settings = {
    Lua = {
      diagnostics = {
        globals = {'vim', 'it', 'describe'}
      },
      runtime = {
        version = "LuaJIT",
        pathStrict = true,
      },
      workspace = {
        library = {
          "${3rd}/luv/library",
          "${3rd}/busted/library",
          "${3rd}/luassert/library",
          home .. "/dev/neovim/neovim/runtime/lua/",
          home .. "/.config/nvim/pack/plugins/start/nvim-dap/lua",
          home .. "/.config/nvim/pack/plugins/start/nvim-fzy/lua",
          home .. "/.config/nvim/pack/plugins/start/nvim-qwahl/lua",
          home .. "/.config/nvim/pack/plugins/start/nvim-lsp-compl/lua",
          home .. "/.config/nvim/pack/plugins/start/nvim-lint/lua",
          home .. "/.config/nvim/pack/plugins/start/nvim-jdtls/lua",
        },
        checkThirdParty = false,
      },
      telemetry = {
        enable = false,
      },
    }
  }
}
vim.lsp.start(config)


local function find_test()
  local bufnr = api.nvim_get_current_buf()
  local lang = "lua"
  local end_row = api.nvim_win_get_cursor(0)[1]
  local query = vim.treesitter.query.parse(lang, [[
    (function_call
      name: (identifier) @name (#any-of? @name "describe" "it")
      arguments: (arguments
        (string) @str
      )
    )
  ]])

  local function get_text(root)
    local start_row = root:range()
    for id, node in query:iter_captures(root, bufnr, start_row, end_row) do
      local name = query.captures[id]
      local text = vim.treesitter.get_node_text(node, bufnr)
      assert(text and type(text) == "string")
      if name == "str" then
        -- strip leading and trailing quotes
        return text:sub(2, #text - 1)
      end
    end
    return nil
  end

  local node = vim.treesitter.get_node()
  local path = {}
  while node ~= nil do
    if node:type() == "function_call" then
      local text = get_text(node)
      if text then
        table.insert(path, text)
      end
    end
    node = node:parent()
  end

  for i = 1, math.floor(#path * 0.5) do
    local tmp = path[i]
    path[i] = path[#path - (i - 1)]
    path[#path - (i - 1)] = tmp
  end

  return path
end

local is_nvim_repo = config.root_dir and vim.endswith(config.root_dir, "neovim/neovim")

vim.keymap.set("n", "<leader>dn", function()
  local path = find_test()
  local bufnr = api.nvim_get_current_buf()
  local lines = api.nvim_buf_get_lines(0, 0, 3, false)
  local lldebugger = {
    'if os.getenv("LOCAL_LUA_DEBUGGER_VSCODE") == "1" then',
    '  require("lldebugger").start()',
    'end',
  }
  local clear_lldebuger = false
  if table.concat(lines, "\n") ~= table.concat(lldebugger, "\n") then
    clear_lldebuger = true
    api.nvim_buf_set_lines(0, 0, 0, true, lldebugger)
    vim.cmd.w()
  elseif vim.bo.modified then
    vim.cmd.w()
  end
  local name = table.concat(path, " ")
  local args
  if is_nvim_repo then
    args = {
      "--ignore-lua",
      "--lazy",
      "--helper=test/functional/preload.lua",
      "--lpath=build/?.lua",
      "--lpath=?.lua",
      api.nvim_buf_get_name(0),
      '--filter="' .. name .. '"',
    }
  else
    args = {
      "--ignore-lua",
      api.nvim_buf_get_name(0),
      '--filter="' .. name .. '"',
    }
  end
  local runconfig = {
    name = name,
    type = "local-lua",
    request = "launch",
    cwd = "${workspaceFolder}",
    program = {
      command = "nbusted",
    },
    args = args
  }
  dap.run(runconfig, {
    after = function()
      if clear_lldebuger then
        clear_lldebuger = false
        api.nvim_buf_set_lines(bufnr, 0, 3, true, {})
        api.nvim_buf_call(bufnr, vim.cmd.w)
      end
    end
  })
end, { buffer = true })

if is_nvim_repo then
  vim.keymap.set("n", "<leader>df", function()
    vim.cmd.split()
    vim.cmd.term(string.format(
      [[TEST_FILE="%s" make functionaltest]],
      api.nvim_buf_get_name(0)
    ))
  end, { buffer = true })
else
  vim.keymap.set("n", "<leader>df", function()
    vim.cmd.split()
    local fname = api.nvim_buf_get_name(0)
    vim.cmd.term('nbusted --ignore-lua "' .. fname .. '"')
  end, { buffer = true })
end
