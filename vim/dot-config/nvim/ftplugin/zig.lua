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
      return text
    end
  end
  return nil
end


local function run_nearest_test()
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
    type = "cppdbg",
    request = "launch",
    program = "/usr/bin/zig",
    args = {"test", "-fno-strip", "${file}", "--test-filter", testcase},
    cwd = "${workspaceFolder}",
  })
end


vim.keymap.set("n", "<leader>dn", run_nearest_test)
