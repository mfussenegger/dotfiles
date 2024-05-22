local lsp = require("me.lsp")
local config = lsp.mk_config {
  cmd = {
    os.getenv('HOME') .. '/dev/microsoft/vscode-gradle/build/gradle-language-server/bin/gradle-language-server'
  },
  name = 'gradle-ls',
  root_dir = vim.fs.root(0, {"gradlew", ".git"}),
  settings = {
    gradleWrapperEnabled = true,
  }
}
vim.lsp.start(config)
