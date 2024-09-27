local api = vim.api


---@return string?
local function find_nearest_test()
  local bufnr = api.nvim_get_current_buf()
  local node = vim.treesitter.get_node()
  while node ~= nil and node:type() ~= "TestDecl" do
    node = node:parent()
  end
  if not node then
    return nil
  end
  for child in node:iter_children() do
    if child:type() == "STRINGLITERALSINGLE" then
      local text = vim.treesitter.get_node_text(child, bufnr, { concat = true })
      assert(type(text) == "string", "concat is true")
      return text:sub(2, #text - 1) -- strip quotes
    end
  end
  return nil
end


local function test_method()
  if vim.bo.modified and vim.bo.buftype == "" then
    vim.cmd.w()
  end
  local testcase = find_nearest_test()
  if not testcase then
    return
  end
  -- TODO: Doesn't stop at breakpoints
  -- Use "--test-no-exec" and then use result binary as `program`?
  require("dap").run({
    name = "Test: " .. testcase,
    type = "gdb",
    request = "launch",
    program = "/usr/bin/zig",
    args = {"test", "-fno-strip", "${file}", "--test-filter", testcase},
    cwd = "${workspaceFolder}",
  })
end

local function test_file()
  if vim.bo.modified and vim.bo.buftype == "" then
    vim.cmd.w()
  end
  require("dap").run({
    name = "Test: " .. api.nvim_buf_get_name(0),
    type = "gdb",
    request = "launch",
    program = "/usr/bin/zig",
    args = {"test", "-fno-strip", "${file}"},
    cwd = "${workspaceFolder}",
  })
end


vim.keymap.set("n", "<leader>dn", test_method)
vim.keymap.set("n", "<leader>df", test_file)


local config = require("me.lsp").mk_config({
  cmd = {"zls"},
  name = "zls",
  root_dir = vim.fs.root(0, {"build.zig", ".git"})
})
vim.lsp.start(config, {
  reuse_client = function (client, conf)
    return (
      client.name == conf.name
      and (
        client.config.root_dir == conf.root_dir
        or (
          conf.root_dir == nil
          and vim.startswith(api.nvim_buf_get_name(0), "/usr/lib/zig/")
        )
      )
    )
  end
})
