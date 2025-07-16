require("config.lazy")

vim.lsp.enable('clangd')
vim.lsp.enable('texlab')

vim.keymap.set(
	"n",
	"<Bslash>j",
	vim.diagnostic.goto_next,
	{ silent = true }
)

vim.keymap.set(
	"n",
	"<Bslash>k",
	vim.diagnostic.goto_prev,
	{ silent = true }
)

vim.keymap.set("t", "<Esc>", "<C-\\><C-n>") -- Enter normal mode on the terminal
vim.keymap.set("n", "<A-j>", vim.cmd.bp) -- Better buffer navigation
vim.keymap.set("n", "<A-k>", vim.cmd.bn) -- Better buffer navigation
vim.keymap.set("n", "<A-d>", vim.cmd.bd) -- Quickly close a buffer
