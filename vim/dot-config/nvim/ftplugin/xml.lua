local home = os.getenv('HOME')
-- See lang-xml playbook for installation
local eclipse = home .. '/dev/eclipse'
local lemminx = eclipse .. '/lemminx/org.eclipse.lemminx/target/org.eclipse.lemminx-uber.jar'
local lemminx_maven = eclipse .. '/lemminx-maven/dist/*.jar'

local cp = {
  lemminx,
}
---@diagnostic disable-next-line: param-type-mismatch
vim.list_extend(cp, vim.split(vim.fn.glob(lemminx_maven, 1), '\n'))

vim.lsp.start(require("me.lsp").mk_config({
  name = "lemminx",
  root_dir = vim.fs.root(0, {'.git'}),
  cmd = {
    os.getenv('JDK17') .. '/bin/java',
    '-cp', table.concat(cp, ':'),
    'org.eclipse.lemminx.XMLServerLauncher'
  },
}))
