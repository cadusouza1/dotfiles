vim.keymap.set("n", "<A-j>", vim.cmd.bp) -- Better buffer navigation
vim.keymap.set("n", "<A-k>", vim.cmd.bn) -- Better buffer navigation
vim.keymap.set("n", "<A-d>", vim.cmd.bd) -- Quickly close a buffer

vim.keymap.set("n", "<A-1>", function()
	vim.cmd.BufferLineGoToBuffer("1")
end)

vim.keymap.set("n", "<A-2>", function()
	vim.cmd.BufferLineGoToBuffer("2")
end)

vim.keymap.set("n", "<A-3>", function()
	vim.cmd.BufferLineGoToBuffer("3")
end)

vim.keymap.set("n", "<A-4>", function()
	vim.cmd.BufferLineGoToBuffer("4")
end)

vim.keymap.set("n", "<A-5>", function()
	vim.cmd.BufferLineGoToBuffer("5")
end)

vim.keymap.set("n", "<A-7>", function()
	vim.cmd.BufferLineGoToBuffer("7")
end)

vim.keymap.set("n", "<A-8>", function()
	vim.cmd.BufferLineGoToBuffer("8")
end)
vim.keymap.set("n", "<A-9>", function()
	vim.cmd.BufferLineGoToBuffer("9")
end)

require("bufferline").setup()
