local api = vim.api
local M = {}

local function exit()
  local key = api.nvim_replace_termcodes("<C-j>", true, false, true)
  api.nvim_feedkeys(key, 'n', true)
end


local snippets = {
  ["*"] = {
    date = function()
      return os.date("%Y-%m-%d") --[[@as string]]
    end,
    uuid = function()
      return vim.trim(vim.fn.system("uuidgen"))
    end,
  },
  lua = {
    f = "local function ${1:name}(${2})\n  $0\nend",
    m = "function ${1:M}${2:.}${3:name}(${4})\n  $0\nend",
    debug = vim.trim([[
if os.getenv("LOCAL_LUA_DEBUGGER_VSCODE") == "1" then
  require("lldebugger").start()
end
]])
  },
  java = {
    f = "${1:private} ${2:static} ${3:void} ${4:name}(${5}) {\n    $0\n}",
    m = "${1:private} ${2:void} ${3:name}(${4}) {\n    $0\n}",
    logger = "private static final Logger LOGGER = LogManager.getLogger(${1}.class);",
    test = "@Test\npublic void ${1:test}() throws Exception {\n    $0\n}",
  },
  ["yaml.ansible"] = {
    git = "ansible.builtin.git:\n  repo: ${1}\n  dest: ${0}",
    pip = "ansible.builtin.pip:\n  name: ${1}\n  virtualenv: ${2}\n  virtualenv_command: /usr/bin/python3 -m venv\n${0}",
    get_url = "ansible.builtin.get_url:\n  url: ${1}\n  dest: ${0}",
    service = "ansible.builtin.service:\n  name: ${1}\n  state: ${2:started}\n  enabled: ${0:true}",
    file = "ansible.builtin.file:\n  path: ${1}\n  state: ${0:absent}",
    dir = "ansible.builtin.file:\n  state: directory\n  recurse: true\n  path: ${0}",
    copy = "ansible.builtin.copy:\n  src: ${1}\n  dest: ${0}",
    package = "ansible.builtin.package:\n  state: ${1:present}\n  name: ${0}",
    ln = "ansible.builtin.file:\n  state: link\n  src: ${1}\n  dest: ${0}",
    command = "ansible.builtin.command: ${1}\nargs:\n  chdir: ${2}\n  creates: ${0}",
  },
  haskell = {
    f = "${1:name} :: ${2}\n${1:name} ${3}= ${4:undefined}${0}",
    lang = "{-# LANGUAGE ${1} #-}$0",
  },
  python = {
    main = 'if __name__ == "__main__":\n    ${1:pass}${0}',
    f = "def ${1:name}(${2}):\n    ${3:pass}${0}",
    m = "def ${1:name}(self${2}):\n    ${3:pass}${0}",
  }
}


-- Expose those in nvim-lsp-compl or add { filter = ..., on_results = ...} options?

---@param item lsp.CompletionItem
---@param defaults lsp.ItemDefaults|nil
local function apply_defaults(item, defaults)
  if not defaults then
    return
  end
  item.insertTextFormat = item.insertTextFormat or defaults.insertTextFormat
  item.insertTextMode = item.insertTextMode or defaults.insertTextMode
  item.data = item.data or defaults.data
  if defaults.editRange then
    local textEdit = item.textEdit or {}
    item.textEdit = textEdit
    textEdit.newText = textEdit.newText or item.textEditText or item.insertText
    if defaults.editRange.start then
      textEdit.range = textEdit.range or defaults.editRange
    elseif defaults.editRange.insert then
      textEdit.insert = defaults.editRange.insert
      textEdit.replace = defaults.editRange.replace
    end
  end
end


--- Extract the completion items from a `textDocument/completion` response
--- and apply defaults
---
---@param result lsp.CompletionItem[]|lsp.CompletionList
---@returns lsp.CompletionItem[]
local function get_completion_items(result)
  if result.items then
    for _, item in pairs(result.items) do
      apply_defaults(item, result.itemDefaults)
    end
    return result.items
  else
    return result
  end
end


function M.maybe()
  local lnum, col = unpack(api.nvim_win_get_cursor(0))
  local line = api.nvim_get_current_line()
  local word = vim.fn.matchstr(line, "\\w\\+\\%.c")
  local ft_snippets = snippets[vim.bo.filetype] or {}
  local snippet = ft_snippets[word]
  if not snippet then
    snippet = snippets["*"][word]
  end
  if snippet then
    api.nvim_buf_set_text(0, lnum - 1, col - #word, lnum - 1, col, {})
    if type(snippet) == "function" then
      snippet = snippet()
    end
    vim.snippet.expand(snippet)
    return
  end

  local clients = vim.lsp.get_clients()
  if not next(clients) then
    return exit()
  end

  if vim.lsp.completion and vim.lsp.completion.get then

    ---@param item lsp.CompletionItem
    local function filter(_, item)
      local kind = vim.lsp.protocol.CompletionItemKind[item.kind] or ''
      return kind == 'Snippet'
    end

    local function on_result(startcol, matches)
      if #matches == 0 then
        return exit()
      else
        vim.fn.complete(startcol, matches)
        if #matches == 1 then
          api.nvim_feedkeys(vim.keycode("<C-n>"), "n", true)
          api.nvim_feedkeys(vim.keycode("<C-y>"), "n", true)
        end
      end
    end
    vim.lsp.completion.get({ filter = filter, on_result = vim.schedule_wrap(on_result) })

    return
  end

  local params = vim.lsp.util.make_position_params(0, "utf-16")
  local results, err = vim.lsp.buf_request_sync(0, 'textDocument/completion', params, 3000)
  assert(not err, vim.inspect(err))
  local mode = api.nvim_get_mode()['mode']
  if mode ~= 'i' and mode ~= 'ic' then
    return
  end
  local matches = {}
  for client_id, resp in pairs(results or {}) do
    local result = resp.result or {}
    local items = get_completion_items(result)
    for _, item in pairs(items) do
      local kind = vim.lsp.protocol.CompletionItemKind[item.kind] or ''
      if kind == 'Snippet' then
        table.insert(matches, {
          word = item.label,
          abbr = item.label,
          kind = 'Snippet',
          menu = item.detail or '',
          icase = 1,
          dup = 1,
          empty = 1,
          user_data = {
            item = item,
            client_id = client_id,
            nvim = {
              lsp = {
                completion_item = item,
                client_id = client_id
              }
            }
          }
        })
      end
    end
  end
  if #matches == 0 then
    return exit()
  end
  local line_to_cursor = line:sub(1, col)
  local word_boundary = vim.fn.match(line_to_cursor, '\\k*$')
  vim.fn.complete(word_boundary + 1, matches)
  if #matches == 1 then
    api.nvim_feedkeys(vim.keycode("<C-n>"), "n", true)
    api.nvim_feedkeys(vim.keycode("<C-y>"), "n", true)
  end
end

return M
