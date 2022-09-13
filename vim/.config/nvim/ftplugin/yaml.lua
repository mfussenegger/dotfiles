local name = vim.api.nvim_buf_get_name(0)
local ansible_patterns = {
  '.*/playbooks/.*%.yml',
  '.*/roles/.*%.yml',
  '.*/playbooks/.*%.yaml',
  '.*/roles/.*%.yaml',
}
for _, pattern in pairs(ansible_patterns) do
  if name:match(pattern) then
    return
  end
end

vim.lsp.start(vim.tbl_extend('force', require('me.lsp').mk_config(), {
  cmd = {'yaml-language-server', '--stdio'},
  name = 'yaml-ls'
}))
