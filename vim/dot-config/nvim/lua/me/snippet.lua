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
    m = "function ${1:M}.${2:name}(${3})\n  $0\nend",
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
    f = "${1:name} :: ${2}\n${1:name} ${3}= ${0:undefined}",
    lang = "{-# LANGUAGE ${1} #-}$0",
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
      ---@diagnostic disable-next-line: param-type-mismatch
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
  local params = vim.lsp.util.make_position_params()
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
            client_id = client_id
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
    api.nvim_feedkeys(
      api.nvim_replace_termcodes("<C-n>", true, false, true), 'n', true)
    api.nvim_feedkeys(
      api.nvim_replace_termcodes("<CR>", true, false, true), 'm', true)
  end
end


function M.paste(reg)
  local source = vim.fn.getreg(reg or "")
  local regtype = vim.fn.getregtype(reg or "")
  local mode = api.nvim_get_mode()

  local lines = vim.split(source, "\n", { plain = true })
  if regtype == "v" then
    -- Strips indentation of subsequent lines.
    -- Selection in normal visual mode is often like this:
    --
    --   ┌── selection start
    --   ▼
    --   first line is intended
    --   second line
    --   third line
    --▲
    --└──── indentation in subsequent lines is part of selection
    --      but undesired for snippet
    local indent = math.huge
    for i, line in ipairs(lines) do
      if i > 1 then
        indent = math.min(line:find("[^ ]") or math.huge, indent)
      end
    end
    indent = indent == math.huge and 0 or indent
    if indent > 0 then
      for i, line in ipairs(lines) do
        if i > 1 then
          lines[i] = line:sub(indent)
        end
      end
    end
    source = table.concat(lines, "\n")
  end

  local parser = vim.treesitter.get_string_parser(source, vim.bo.filetype)
  local trees = parser:parse()
  local root = trees[1]:root()
  if not root then
    return
  end
  local query = vim.treesitter.query.get(parser:lang(), "tabstop")
  assert(query, "Must have a tabstop query file")

  ---@type table<integer, TSNode>
  local leafs = {}
  local nodes = {}
  for id, node, _, _ in query:iter_captures(root, source) do
    if query.captures[id] == "tabstop" then
      leafs[node:id()] = node
      table.insert(nodes, node)
    end
  end

  --- Prune any non-leafs; nested snippet placeholders are not supported
  for _, node in pairs(leafs) do
    local parent = node:parent()
    while parent ~= nil do
      leafs[parent:id()] = nil
      parent = parent:parent()
    end
  end

  local function is_leaf(n)
    return leafs[n:id()] ~= nil
  end

  for i, n in vim.iter(nodes):rev():filter(is_leaf):enumerate() do
    local slnum, scol, elnum, ecol = n:range()
    if slnum == elnum then
      local lnum = slnum + 1
      local line = lines[lnum]
      local text = line:sub(scol + 1, ecol):gsub("[%$%}]", "\\%1")
      lines[lnum] = table.concat({
        line:sub(1, scol),
        string.format("${%s:%s}", i, text),
        line:sub(ecol + 1)
      })
    end
  end
  -- this should kinda emulate a `yy` + `p` where the paste happens in the next line
  if mode.mode == "n" and regtype == "V" then
    if lines[#lines] == "" then
      table.remove(lines)
    end
    vim.cmd.normal("o")
  end
  local snippet = table.concat(lines, "\n")
  vim.snippet.expand(snippet)
end


return M
