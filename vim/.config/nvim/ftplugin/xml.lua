local home = os.getenv('HOME')
-- See lang-xml playbook for installation
local eclipse = home .. '/dev/eclipse'
local lemminx = eclipse .. '/lemminx/org.eclipse.lemminx/target/org.eclipse.lemminx-uber.jar'
local lemminx_maven = eclipse .. '/lemminx-maven/dist/*.jar'

local lsp = require('me.lsp')
local config = lsp.mk_config()
local cp = {
  lemminx,
}
vim.list_extend(cp, vim.split(vim.fn.glob(lemminx_maven, 1), '\n'))
config.name = 'lemminx'
config.cmd = {
  os.getenv('JDK17') .. '/bin/java',
  '-cp',
  table.concat(cp, ':'),
  'org.eclipse.lemminx.XMLServerLauncher'
}
config.root_dir = lsp.find_root({'.git'})
vim.lsp.start(config)
