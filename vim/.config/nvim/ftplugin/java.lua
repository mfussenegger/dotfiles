local jdtls = require('jdtls')
local root_markers = {'gradlew', '.git'}
local root_dir = require('jdtls.setup').find_root(root_markers)
local home = os.getenv('HOME')
local workspace_folder = home .. "/.local/share/eclipse/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
local config = require('me.lsp.conf').mk_config()
config.settings = {
  java = {
    signatureHelp = { enabled = true };
    contentProvider = { preferred = 'fernflower' };
    completion = {
      favoriteStaticMembers = {
        "org.assertj.core.api.Assertions.assertThat",
        "org.assertj.core.api.Assertions.assertThatThrownBy",
        "org.assertj.core.api.Assertions.assertThatExceptionOfType",
        "org.assertj.core.api.Assertions.catchThrowable",
        "org.hamcrest.MatcherAssert.assertThat",
        "org.hamcrest.Matchers.*",
        "org.hamcrest.CoreMatchers.*",
        "org.junit.jupiter.api.Assertions.*",
        "java.util.Objects.requireNonNull",
        "java.util.Objects.requireNonNullElse",
        "org.mockito.Mockito.*"
      },
      filteredTypes = {
        "com.sun.*",
        "io.micrometer.shaded.*",
        "java.awt.*",
        "jdk.*",
        "sun.*",
      },
    };
    sources = {
      organizeImports = {
        starThreshold = 9999;
        staticStarThreshold = 9999;
      };
    };
    codeGeneration = {
      toString = {
        template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}"
      },
      hashCodeEquals = {
        useJava7Objects = true,
      },
      useBlocks = true,
    };
    configuration = {
      runtimes = {
        {
          name = "JavaSE-1.8",
          path = "/usr/lib/jvm/java-8-openjdk/",
        },
        {
          name = "JavaSE-11",
          path = "/usr/lib/jvm/java-11-openjdk/",
        },
        {
          name = "JavaSE-17",
          path = home .. "/.local/jdks/jdk-17.0.4+8/",
        },
        {
          name = "JavaSE-18",
          path = home .. "/.local/jdks/jdk-18.0.2+9/",
        },
      }
    };
  };
}
config.cmd = {
  home .. "/.local/jdks/jdk-18.0.2+9/bin/java",
  '-Declipse.application=org.eclipse.jdt.ls.core.id1',
  '-Dosgi.bundles.defaultStartLevel=4',
  '-Declipse.product=org.eclipse.jdt.ls.core.product',
  '-Dlog.protocol=true',
  '-Dlog.level=ALL',
  '-Xmx4g',
  '--add-modules=ALL-SYSTEM',
  '--add-opens', 'java.base/java.util=ALL-UNNAMED',
  '--add-opens', 'java.base/java.lang=ALL-UNNAMED',
  '-jar', vim.fn.glob(home .. '/dev/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_*.jar'),
  '-configuration', home .. '/dev/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/config_linux',
  '-data', workspace_folder,
}
config.on_attach = function(client, bufnr)
  require('me.lsp.conf').on_attach(client, bufnr, {
    server_side_fuzzy_completion = true,
  })

  jdtls.setup_dap({hotcodereplace = 'auto'})
  jdtls.setup.add_commands()
  local opts = { silent = true, buffer = bufnr }
  vim.keymap.set('n', "<A-o>", jdtls.organize_imports, opts)
  vim.keymap.set('n', "<leader>df", jdtls.test_class, opts)
  vim.keymap.set('n', "<leader>dn", jdtls.test_nearest_method, opts)

  vim.keymap.set('n', "<leader>dN",
    function()
      local async_profiler_so = home .. "/apps/async-profiler/build/libasyncProfiler.so"
      local vmArgs = "-ea -agentpath:" .. async_profiler_so .. "=start,event=cpu,alloc=2m,lock=10ms,file=/tmp/profile.jfr"
      jdtls.test_nearest_method({
        config_overrides = {
          vmArgs = vmArgs,
          noDebug = true,
        },
        after_test = function()
          vim.fn.system("jfr2flame /tmp/profile.jfr /tmp/profile.html")
          vim.fn.system("firefox /tmp/profile.html")
        end
      })
    end,
  opts)

  vim.keymap.set('n', "crv", jdtls.extract_variable, opts)
  vim.keymap.set('v', "crv", [[<ESC><CMD>lua require('jdtls').extract_variable(true)<CR>]], opts)
  vim.keymap.set('v', 'crm', [[<ESC><CMD>lua require('jdtls').extract_method(true)<CR>]], opts)
  vim.keymap.set('n', "crc", jdtls.extract_constant, opts)
  local create_command = vim.api.nvim_buf_create_user_command
  create_command(bufnr, 'W', require('me.lsp.ext').remove_unused_imports, {
    nargs = 0,
  })
end

local jar_patterns = {
  '/dev/microsoft/java-debug/com.microsoft.java.debug.plugin/target/com.microsoft.java.debug.plugin-*.jar',
  '/dev/dgileadi/vscode-java-decompiler/server/*.jar',
  '/dev/microsoft/vscode-java-test/java-extension/com.microsoft.java.test.plugin/target/*.jar',
  '/dev/microsoft/vscode-java-test/java-extension/com.microsoft.java.test.runner/target/*.jar',
  '/dev/microsoft/vscode-java-test/java-extension/com.microsoft.java.test.runner/lib/*.jar',
  '/dev/testforstephen/vscode-pde/server/*.jar'
}
-- npm install broke for me: https://github.com/npm/cli/issues/2508
-- So gather the required jars manually; this is based on the gulpfile.js in the vscode-java-test repo
local plugin_path = '/dev/microsoft/vscode-java-test/java-extension/com.microsoft.java.test.plugin.site/target/repository/plugins/'
local bundle_list = vim.tbl_map(
  function(x) return require('jdtls.path').join(plugin_path, x) end,
  {
    'org.eclipse.jdt.junit4.runtime_*.jar',
    'org.eclipse.jdt.junit5.runtime_*.jar',
    'org.junit.jupiter.api*.jar',
    'org.junit.jupiter.engine*.jar',
    'org.junit.jupiter.migrationsupport*.jar',
    'org.junit.jupiter.params*.jar',
    'org.junit.vintage.engine*.jar',
    'org.opentest4j*.jar',
    'org.junit.platform.commons*.jar',
    'org.junit.platform.engine*.jar',
    'org.junit.platform.launcher*.jar',
    'org.junit.platform.runner*.jar',
    'org.junit.platform.suite.api*.jar',
    'org.apiguardian*.jar'
  }
)
vim.list_extend(jar_patterns, bundle_list)
local bundles = {}
for _, jar_pattern in ipairs(jar_patterns) do
  for _, bundle in ipairs(vim.split(vim.fn.glob(home .. jar_pattern), '\n')) do
    if not vim.endswith(bundle, 'com.microsoft.java.test.runner-jar-with-dependencies.jar')
      and not vim.endswith(bundle, 'com.microsoft.java.test.runner.jar') then
      table.insert(bundles, bundle)
    end
  end
end
local extendedClientCapabilities = jdtls.extendedClientCapabilities;
extendedClientCapabilities.resolveAdditionalTextEditsSupport = true;
config.init_options = {
  bundles = bundles;
  extendedClientCapabilities = extendedClientCapabilities;
}
-- mute; having progress reports is enough
config.handlers['language/status'] = function() end
jdtls.start_or_attach(config)
