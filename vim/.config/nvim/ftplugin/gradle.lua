local config = vim.tbl_extend('force', require('me.lsp').mk_config(), {
  cmd = {
    os.getenv('HOME') .. '/dev/microsoft/vscode-gradle/build/gradle-language-server/bin/gradle-language-server'
  },
  name = 'gradle-ls',
  cmd_env = {
    JAVA_HOME = '/usr/lib/jvm/java-11-openjdk'
  },
  root_dir = vim.fs.dirname(vim.fs.find({'gradlew', '.git'}, { upward = true })[1]),
  settings = {
    gradleWrapperEnabled = true,
  }
})
vim.lsp.start(config)
