local M = {}
local lsp = vim.lsp
local api = vim.api


---@param config vim.lsp.ClientConfig
---@return vim.lsp.ClientConfig
function M.mk_config(config)
  local lsp_compl = require('lsp_compl')
  local capabilities = vim.tbl_deep_extend(
    "force",
    lsp.protocol.make_client_capabilities(),
    lsp_compl.capabilities(),
    {
      workspace = {
        didChangeWatchedFiles = {
          dynamicRegistration = true
        }
      },
    }
  )
  local defaults = {
    handlers = {},
    capabilities = capabilities,
    init_options = vim.empty_dict(),
    settings = vim.empty_dict(),
  }
  if config then
    return vim.tbl_deep_extend("force", defaults, config)
  else
    return defaults
  end
end


---@param config vim.lsp.Config
local function enable(name, config)
  if lsp.config then
    lsp.config(name, config)
    lsp.enable(name)
    return
  end
  local group = api.nvim_create_augroup("lsp-enable-" .. name, { clear = true })
  for _, ft in ipairs(config.filetypes) do
    api.nvim_create_autocmd("FileType", {
      pattern = ft,
      group = group,

      ---@param args vim.api.keyset.create_autocmd.callback_args
      callback = function(args)
        if config.root_markers then
          config = vim.deepcopy(config)
          config.root_dir = vim.fs.root(args.buf, config.root_markers)
        end
        vim.lsp.start(config, {
          bufnr = args.buf,
          reuse_client = config.reuse_client,
        })
      end,
    })
  end
end

---@param client vim.lsp.Client
local function extendtriggers(client)
  local triggers = vim.tbl_get(client.server_capabilities, "completionProvider", "triggerCharacters")
  if not triggers then
    return
  end
  for _, char in ipairs({"a", "e", "i", "o", "u"}) do
    if not vim.tbl_contains(triggers, char) then
      table.insert(triggers, char)
    end
  end
  for i, t in ipairs(triggers) do
    if t == "," then
      triggers[i] = nil
    end
  end
  client.server_capabilities.completionProvider.triggerCharacters = vim.iter(triggers):totable()
end


function M.setup()
  enable("json-ls", {
    cmd = {"vscode-json-language-server", "--stdio"},
    filetypes = {"json"}
  })
  enable("html-ls", {
    cmd = {"vscode-html-language-server", "--stdio"},
    filetypes = {"html", "htmldjango"}
  })
  enable("css-ls", {
    cmd = {"vscode-css-language-server", "--stdio"},
    filetypes = {"css"}
  })
  enable("bash-ls", {
    cmd = {"bash-language-server", "start"},
    filetypes = {"sh"}
  })
  enable("ts-ls", {
    cmd = {"typescript-language-server", "--stdio"},
    filetypes = {"javascript", "typescript"},
    root_markers = {"package.json", ".git"},
  })

  local setk = vim.keymap.set
  setk("n", "gr", function() vim.lsp.buf.references({ includeDeclaration = false }) end)
  setk({"n", "x"}, "<a-CR>", vim.lsp.buf.code_action)
  setk({"n", "x"}, "<leader>r", "<Cmd>lua vim.lsp.buf.code_action { context = { only = {'refactor'} }}<CR>")
  setk("n", "crn", "<Cmd>lua vim.lsp.buf.rename(vim.fn.input('New Name: '))<CR>")
  setk("n", "<leader>cr", function() vim.lsp.codelens.refresh({ bufnr = 0 }) end)
  setk("n", "<leader>ce", vim.lsp.codelens.run)

  local lspgroup = api.nvim_create_augroup("lsp", {})
  api.nvim_create_autocmd("LspAttach", {
    group = lspgroup,
    ---@param args vim.api.keyset.create_autocmd.callback_args
    callback = function(args)
      local client = assert(vim.lsp.get_client_by_id(args.data.client_id))

      if client.server_capabilities.completionProvider then
        extendtriggers(client)
        if vim.lsp.completion then
          vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
        else
          require("lsp_compl").attach(client, args.buf, {})
        end
      end
      if client.server_capabilities.implementationProvider then
        setk("n", "gD", vim.lsp.buf.implementation, { buffer = args.buf })
      end

      if client.server_capabilities.codeLensProvider then
        local bufnr = args.buf
        local cl_group_name = string.format("lsp-codelens-%d", bufnr)

        local function autorefresh()
          vim.lsp.codelens.refresh({ bufnr = bufnr })
          api.nvim_create_autocmd({"InsertLeave", "CursorHold"}, {
            group = api.nvim_create_augroup(cl_group_name, { clear = true }),
            buffer = bufnr,
            callback = function()
              vim.lsp.codelens.refresh({ bufnr = bufnr })
            end,
          })
        end

        local function clear()
          if vim.lsp.codelens.clear then
            vim.lsp.codelens.clear(nil, bufnr)
          end
          pcall(api.nvim_del_augroup_by_name, cl_group_name)
        end

        setk("n", "<leader>ca", autorefresh, { buffer = bufnr })
        setk("n", "<leader>cc", clear, { buffer = bufnr })
      end

      if client.server_capabilities.documentHighlightProvider then
        local group = api.nvim_create_augroup(string.format("lsp-%s-%s", args.buf, args.data.client_id), {})
        api.nvim_create_autocmd("CursorHold", {
          group = group,
          buffer = args.buf,
          callback = function()
            local params = vim.lsp.util.make_position_params(0, client.offset_encoding)
            ---@diagnostic disable-next-line: param-type-mismatch
            client.request("textDocument/documentHighlight", params, nil, args.buf)
          end,
        })
        api.nvim_create_autocmd("CursorMoved", {
          group = group,
          buffer = args.buf,
          callback = function()
            pcall(vim.lsp.util.buf_clear_references, args.buf)
          end,
        })
      end
    end,
  })
  api.nvim_create_autocmd("LspDetach", {
    group = lspgroup,
    callback = function(args)
      local group = api.nvim_create_augroup(string.format("lsp-%s-%s", args.buf, args.data.client_id), {})
      pcall(api.nvim_del_augroup_by_name, group)
    end,
  })

  local timer = vim.loop.new_timer()
  api.nvim_create_autocmd("LspProgress", {
    group = lspgroup,
    callback = function()
      if api.nvim_get_mode().mode == "n" then
        vim.cmd.redrawstatus()
      end
      if timer then
        timer:stop()
        timer:start(500, 0, vim.schedule_wrap(function()
          timer:stop()
          vim.cmd.redrawstatus()
        end))
      end
    end,
  })


  api.nvim_create_user_command(
    'LspStop',
    function(kwargs)
      local name = kwargs.fargs[1]
      for _, client in ipairs(lsp.get_clients({ name = name })) do
        client:stop()
      end
    end,
    {
      nargs = "?",
      complete = function()
        return vim.tbl_map(function(c) return c.name end, lsp.get_clients())
      end
    }
  )
  api.nvim_create_user_command(
    "LspRestart",
    function(kwargs)
      local name = kwargs.fargs[1]
      for _, client in ipairs(lsp.get_clients({ name = name })) do
        local bufs = lsp.get_buffers_by_client_id(client.id)
        client:stop()
        vim.wait(30000, function()
          return lsp.get_client_by_id(client.id) == nil
        end)
        local client_id = lsp.start(client.config, { attach = false })
        if client_id then
          for _, buf in ipairs(bufs) do
            lsp.buf_attach_client(buf, client_id)
          end
        end
      end
    end,
    {
      nargs = "?",
      complete = function()
        return vim.tbl_map(function(c) return c.name end, lsp.get_clients())
      end
    }
  )
end


return M

