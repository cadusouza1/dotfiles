local dap = require("dap")
local dapui = require("dapui")

require("dapui").setup()

dap.listeners.after.event_initialized["dapui_config"] = function()
	dapui.open()
end

dap.listeners.before.event_terminated["dapui_config"] = function()
	dapui.close()
end

dap.listeners.before.event_exited["dapui_config"] = function()
	dapui.close()
end

vim.keymap.set("n", "<leader>b", require("dap").toggle_breakpoint)
vim.keymap.set("n", "<leader>c", require("dap").continue)
vim.keymap.set("n", "<leader>o", require("dap").step_over)
vim.keymap.set("n", "<leader>i", require("dap").step_into)
-- vim.keymap.set("n", "", require'dap'.repl.open)

dap.adapters.codelldb = {
	type = "server",
	port = "${port}",
	executable = {
		-- CHANGE THIS to your path!
		command = vim.fn.stdpath("data") .. "/mason/packages/codelldb/codelldb",
		args = { "--port", "${port}" },

		-- On windows you may have to uncomment this:
		-- detached = false,
	},
}

dap.configurations.cpp = {
	{
		name = "Launch file",
		type = "codelldb",
		request = "launch",
		program = function()
			return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
		end,
		cwd = "${workspaceFolder}",
		stopOnEntry = false,
	},
}

dap.configurations.c = dap.configurations.cpp
dap.configurations.rust = dap.configurations.cpp

-- require("dap").adapters.c = {
-- 	type = "executable",
-- 	command = vim.fn.stdpath("data") .. "/mason/packages/codelldb/codelldb",
-- }