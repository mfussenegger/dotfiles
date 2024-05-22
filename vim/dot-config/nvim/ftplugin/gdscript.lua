if vim.lsp.rpc.connect then
  local config = require('me.lsp').mk_config()
  config.cmd = vim.lsp.rpc.connect('127.0.0.1', 6008)
  config.root_dir = vim.fs.root(0, {'.git', 'project.godot'})
  vim.lsp.start(config)
end

local dap = require('dap')
dap.adapters.godot = {
  type = "server",
  host = '127.0.0.1',
  port = 6006,
}
dap.configurations.gdscript = {
  {
    type = "godot",
    request = "launch",
    name = "Launch scene",
    project = "${workspaceFolder}",
    launch_scene = true,
  }
}
