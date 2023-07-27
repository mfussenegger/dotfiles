local api = vim.api
local M = {}

local function exit()
  local key = api.nvim_replace_termcodes("<C-j>", true, false, true)
  api.nvim_feedkeys(key, 'n', true)
end


local function setup()
  local ls = require('luasnip')
  local s = ls.s
  local p = require("luasnip.extras").partial
  local parse_snippet = ls.parser.parse_snippet
  ls.add_snippets(nil,  {
    all = {
      s('date', p(os.date, '%Y-%m-%d')),
      s("uuid", p(function()
        return vim.trim(vim.fn.system("uuidgen"))
      end)),
    },
    lua = {
      parse_snippet('f', 'local function ${1:name}(${2})\n  $0\nend'),
      parse_snippet('m', 'function ${1:M}.${2:name}(${3})\n  $0\nend'),
    },
    java = {
      parse_snippet('f', '${1:private} ${2:static} ${3:void} ${4:name}(${5}) {\n    $0\n}'),
      parse_snippet('m', '${1:private} ${2:void} ${3:name}(${4}) {\n    $0\n}'),
      parse_snippet('logger', 'private static final Logger LOGGER = LogManager.getLogger(${1}.class);'),
      parse_snippet('test', '@Test\npublic void ${1:test}() throws Exception {\n    $0\n}'),
    },
    ansible = {
      parse_snippet('git', 'ansible.builtin.git:\n  repo: ${1}\n  dest: ${0}'),
      parse_snippet('pip', 'ansible.builtin.pip:\n  name: ${1}\n  virtualenv: ${2}\n  virtualenv_command: /usr/bin/python3 -m venv\n${0}'),
      parse_snippet('get_url', 'ansible.builtin.get_url:\n  url: ${1}\n  dest: ${0}'),
      parse_snippet('service', 'ansible.builtin.service:\n  name: ${1}\n  state: ${2:started}\n  enabled: ${0:true}'),
      parse_snippet('file', 'ansible.builtin.file:\n  path: ${1}\n  state: ${0:absent}'),
      parse_snippet('dir', 'ansible.builtin.file:\n  state: directory\n  recurse: true\n  path: ${0}'),
      parse_snippet('copy', 'ansible.builtin.copy:\n  src: ${1}\n  dest: ${0}'),
      parse_snippet('package', 'ansible.builtin.package:\n  state: ${1:present}\n  name: ${0}'),
      parse_snippet('ln', 'ansible.builtin.file:\n  state: link\n  src: ${1}\n  dest: ${0}'),
      parse_snippet('command', 'ansible.builtin.command: ${1}\nargs:\n  chdir: ${2}\n  creates: ${0}'),
    },
    haskell = {
      parse_snippet('f', '${1:name} :: ${2}\n${1:name} ${3}= ${0:undefined}'),
      parse_snippet('lang', '{-# LANGUAGE ${1} #-}$0'),
    },
  })
end

local loaded = false



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
  if not loaded then
    loaded = true
    setup()
  end
  local ls = require('luasnip')
  local expandable = ls.expandable()
  if expandable then
    ls.expand({})
  else
    local clients = vim.lsp.get_active_clients()
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
    local cursor_pos = api.nvim_win_get_cursor(0)[2]
    local line = api.nvim_get_current_line()
    local line_to_cursor = line:sub(1, cursor_pos)
    local col = vim.fn.match(line_to_cursor, '\\k*$')
    vim.fn.complete(col + 1, matches)
    if #matches == 1 then
      api.nvim_feedkeys(
        api.nvim_replace_termcodes("<C-n>", true, false, true), 'n', true)
      api.nvim_feedkeys(
        api.nvim_replace_termcodes("<CR>", true, false, true), 'm', true)
    end
  end
end

return M
