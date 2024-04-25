local api = vim.api
local M = {}
local ns = api.nvim_create_namespace('gh')


function M.comments()
  local branch = vim.trim(vim.fn.system("git branch --show current"))
  local pr_list = vim.fn.system('gh pr list --head "' .. branch .. '" --json number')
  local prs = assert(vim.json.decode(pr_list), "gh pr list must have a result that decodes")
  local comments_cmd = 'gh api "repos/{owner}/{repo}/pulls/' .. prs[1].number .. '/comments"'
  local comments = vim.json.decode(vim.fn.system(comments_cmd), { luanil = { object = true }})
  assert(comments, "gh api ... should have json list result")
  local buf_diagnostic = vim.defaulttable()
  for _, comment in pairs(comments) do
    if comment.line then
      local path = comment.path
      local bufnr = vim.fn.bufadd(path)
      table.insert(buf_diagnostic[bufnr], {
        bufnr = bufnr,
        lnum = comment.line - 1,
        col = 0,
        message = comment.body,
        severity = vim.diagnostic.severity.WARN
      })
    end
  end
  local qflist = {}
  for bufnr, diagnostic in pairs(buf_diagnostic) do
    local list = vim.diagnostic.toqflist(diagnostic)
    vim.list_extend(qflist, list)
    vim.diagnostic.set(ns, bufnr, diagnostic)
  end
  vim.fn.setqflist(qflist, 'r')
  vim.cmd.copen()
end


function M.clear()
  vim.diagnostic.reset(ns)
end


return M
