local lsp = require("me.lsp")
local config = lsp.mk_config {
  cmd = {
    os.getenv('HOME') .. '/dev/microsoft/vscode-gradle/build/gradle-language-server/bin/gradle-language-server'
  },
  name = 'gradle-ls',
  root_dir = lsp.find_root({"gradlew", ".git"}),
  settings = {
    gradleWrapperEnabled = true,
  }
}
vim.lsp.start(config)
