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
