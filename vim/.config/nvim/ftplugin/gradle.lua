local cmd = {
  os.getenv('HOME') .. '/dev/microsoft/vscode-gradle/build/gradle-language-server/bin/gradle-language-server'
}
local config = {
  name = 'gradle-ls',
  cmd_env = {
    JAVA_HOME = '/usr/lib/jvm/java-11-openjdk'
  },
  settings = {
    gradleWrapperEnabled = true,
  }
}
require('me.lsp.conf').add_client(cmd, config)
