local dap = require('dap')
local api = vim.api
dap.adapters.nlua = function(callback, conf)
  local port = conf["port"]
  local adapter = {
    type = 'server',
    host = '127.0.0.1',
    port = port
  }
  if conf.start_neovim then
    local start_opts = conf.start_neovim
    conf.start_neovim = nil
    local handle
    local pid_or_err
    local address = string.format('/tmp/nvim-osv-%d', port)
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
    end, 500)
  else
    callback(adapter)
  end
end

dap.adapters["local-lua"] = {
  type = 'executable',
  command = 'local-lua-dbg',
  enrich_config = function(config, on_config)
    if not config["extensionPath"] then
      local c = vim.deepcopy(config)
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
    type = "nlua",
    request = "attach",
    name = "New instance (current file)",
    port = free_port,
    start_neovim = {}
  },
  {
    type = "nlua",
    request = "attach",
    name = "New instance (dotfiles)",
    port = free_port,
    start_neovim = {
      cwd = os.getenv('HOME') .. '/dotfiles',
      fname = 'vim/.config/nvim/init.lua',
    }
  },
  {
    type = "nlua",
    request = "attach",
    name = "New instance (crate/crate)",
    port = free_port,
    start_neovim = {
      cwd = os.getenv('HOME') .. '/dev/crate/crate',
      fname = 'server/src/test/java/io/crate/planner/PlannerTest.java',
    }
  },
  {
    type = "nlua",
    request = "attach",
    name = "New instance (neovim/neovim)",
    port = free_port,
    start_neovim = {
      cwd = os.getenv('HOME') .. '/dev/neovim/neovim',
      fname = 'src/nvim/main.c',
    }
  },
  {
    type = 'nlua',
    request = 'attach',
    name = 'Attach',
    port = function()
      return assert(tonumber(vim.fn.input('Port: ')), 'Port is required')
    end
  },
  {
    name = 'Current file (local-lua-dbg, nlua)',
    type = 'local-lua',
    request = 'launch',
    cwd = '${workspaceFolder}',
    program = {
      lua = 'nlua.lua',
      file = '${file}',
    },
    verbose = true,
    args = {},
  },
  {
    name = 'Busted current file (local-lua-dbg, nlua)',
    type = 'local-lua',
    request = 'launch',
    cwd = '${workspaceFolder}',
    program = {
      command = "busted",
    },
    verbose = true,
    args = {
      "${file}"
    },
  },
  {
    name = 'Current file (local-lua-dbg, lua)',
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
          os.getenv("HOME") .. "/dev/neovim/neovim/runtime/lua/",
          os.getenv("HOME") .. "/.config/nvim/pack/plugins/start/nvim-dap/lua",
          os.getenv("HOME") .. "/.config/nvim/pack/plugins/start/nvim-fzy/lua",
          os.getenv("HOME") .. "/.config/nvim/pack/plugins/start/nvim-qwahl/lua",
          os.getenv("HOME") .. "/.config/nvim/pack/plugins/start/nvim-lsp-compl/lua",
          os.getenv("HOME") .. "/.config/nvim/pack/plugins/start/nvim-lint/lua",
          os.getenv("HOME") .. "/.config/nvim/pack/plugins/start/nvim-jdtls/lua",
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


if vim.loop.fs_stat("lemmy.sh") then
  local bufnr = api.nvim_get_current_buf()
  local group = vim.api.nvim_create_augroup('lemmy-' .. bufnr, { clear = true })
  api.nvim_create_autocmd('BufWritePost', {
    command = 'silent !./lemmy.sh',
    buffer = bufnr,
    group = group
  })
end

local function find_test()
    local bufnr = api.nvim_get_current_buf()
    local lang = "lua"
    local end_row = api.nvim_win_get_cursor(0)[1]
    local query = vim.treesitter.query.parse(lang, [[
      ((function_call
        name: (identifier) @name (#any-of? @name "describe" "it")
        arguments: (arguments
          ((string) @str)
        )
      ))
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


if config.root_dir and vim.endswith(config.root_dir, "neovim/neovim") then
  local function run_test()
    local path = find_test()
    vim.cmd.split()
    local fname = api.nvim_buf_get_name(0)
    local filter = table.concat(path, " ")
    vim.cmd.term(string.format(
      [[TEST_FILE="%s" make functionaltest TEST_FILTER="%s"]],
      fname,
      filter
    ))
  end

  vim.keymap.set("n", "<leader>dn", run_test)
  vim.keymap.set("n", "<leader>df", function()
    vim.cmd.split()
    vim.cmd.term(string.format(
      [[TEST_FILE="%s" make functionaltest]],
      api.nvim_buf_get_name(0)
    ))
  end)
else
  vim.keymap.set("n", "<leader>dn", function()
    if vim.bo.modified then
      vim.cmd.w()
    end
    local path = find_test()
    local name = table.concat(path, " ")
    dap.run({
      name = name,
      type = "local-lua",
      request = "launch",
      cwd = "${workspaceFolder}",
      program = {
        command = "busted",
      },
      args = {
        api.nvim_buf_get_name(0),
        '--filter="' .. name .. '"',
      }
    })
  end)
  vim.keymap.set("n", "<leader>df", function()
    vim.cmd.split()
    local fname = api.nvim_buf_get_name(0)
    vim.cmd.term('~/.luarocks/bin/busted "' .. fname .. '"')
  end)
end
