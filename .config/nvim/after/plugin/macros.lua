vim.keymap.set(
	"n",
	"<leader>md",
	":%s/\\\\(\\|\\\\)/$/g<cr>:%s/\\\\\\[\\|\\\\\\]/$$/g<cr>"
)
