local api = vim.api
local jdtls = require('jdtls')
local root_markers = {'gradlew', '.git'}
local root_dir = require('jdtls.setup').find_root(root_markers)
local home = os.getenv('HOME')
local workspace_folder = home .. "/.local/share/eclipse/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
local config = require('me.lsp').mk_config()
config.settings = {
  java = {
    autobuild = { enabled = false },
    signatureHelp = { enabled = true };
    contentProvider = { preferred = 'fernflower' };
    saveActions = {
      organizeImports = true,
    },
    completion = {
      favoriteStaticMembers = {
        "io.crate.testing.Asserts.assertThat",
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
        "org.mockito.Mockito.*",
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
          name = "JavaSE-17",
          path = os.getenv("JDK17"),
        },
        {
          name = "JavaSE-18",
          path = home .. "/.m2/jdks/jdk-18.0.2.1+1/",
        },
        {
          name = "JavaSE-19",
          path = os.getenv("JDK19"),
        },
        {
          name = "JavaSE-20",
          path = os.getenv("JDK20"),
        },
      }
    };
  };
}
config.cmd = {
  home .. "/.m2/jdks/jdk-18.0.2.1+1/bin/java",
  --'-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=1044',
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


local function test_with_profile(test_fn)
  return function()
    if vim.bo.modified then
      vim.cmd.w()
    end
    local choices = {
      'cpu,alloc=2m,lock=10ms',
      'cpu',
      'alloc',
      'wall',
      'context-switches',
      'cycles',
      'instructions',
      'cache-misses',
    }
    local select_opts = {
      format_item = tostring
    }
    vim.ui.select(choices, select_opts, function(choice)
      if not choice then
        return
      end
      local async_profiler_so = home .. "/apps/async-profiler/build/libasyncProfiler.so"
      local event = 'event=' .. choice
      local vmArgs = "-ea -agentpath:" .. async_profiler_so .. "=start,"
      vmArgs = vmArgs .. event .. ",file=/tmp/profile.jfr"
      test_fn({
        config_overrides = {
          vmArgs = vmArgs,
          noDebug = true,
        },
        after_test = function()
          vim.fn.system("jfr2flame /tmp/profile.jfr /tmp/profile.html")
          vim.fn.system("firefox /tmp/profile.html")
        end
      })
    end)
  end
end

config.on_attach = function(client, bufnr)
  api.nvim_create_autocmd("BufWritePost", {
    buffer = bufnr,
    callback = function()
      client.request_sync("java/buildWorkspace", false, 5000, bufnr)
    end,
  })

  api.nvim_buf_create_user_command(bufnr, "A", function()
    require("jdtls.tests").goto_subjects()
  end, {})

  require('lsp_compl').attach(client, bufnr, {
    server_side_fuzzy_completion = true,
  })

  local opts = { silent = true, buffer = bufnr }
  local set = vim.keymap.set
  set('n', "<A-o>", jdtls.organize_imports, opts)
  set('n', "<leader>df", function()
    if vim.bo.modified then
      vim.cmd('w')
    end
    jdtls.test_class()
  end, opts)
  set('n', "<leader>dl", function()
    if vim.bo.modified then
      vim.cmd('w')
    end
    require("dap").run_last()
  end)
  set('n', "<leader>dF", test_with_profile(jdtls.test_class), opts)
  set('n', "<leader>dn", function()
    if vim.bo.modified then
      vim.cmd('w')
    end
    jdtls.test_nearest_method({
      config_overrides = {
        stepFilters = {
          skipClasses = {"$JDK", "junit.*"},
          skipSynthetics = true
        }
      }
    })
  end, opts)
  set('n', "<leader>dN", test_with_profile(jdtls.test_nearest_method), opts)

  vim.keymap.set('n', "crv", jdtls.extract_variable_all, opts)
  vim.keymap.set('v', "crv", [[<ESC><CMD>lua require('jdtls').extract_variable_all(true)<CR>]], opts)
  vim.keymap.set('v', 'crm', [[<ESC><CMD>lua require('jdtls').extract_method(true)<CR>]], opts)
  vim.keymap.set('n', "crc", jdtls.extract_constant, opts)
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
    'junit-jupiter-*.jar',
    'junit-platform-*.jar',
    'junit-vintage-engine_*.jar',
    'org.opentest4j*.jar',
    'org.apiguardian.api_*.jar',
    'org.eclipse.jdt.junit4.runtime_*.jar',
    'org.eclipse.jdt.junit5.runtime_*.jar',
    'org.opentest4j_*.jar'
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
extendedClientCapabilities.onCompletionItemSelectedCommand = "editor.action.triggerParameterHints"
config.init_options = {
  bundles = bundles;
  extendedClientCapabilities = extendedClientCapabilities;
}
-- mute; having progress reports is enough
config.handlers['language/status'] = function() end
jdtls.start_or_attach(config)
