local util = require("formatter.util")

local clangformat = function()
	return {
		exe = "clang-format",
		args = {
			"-style={IndentWidth: 4}",
			"-assume-filename",
			util.escape_path(util.get_current_buffer_file_name()),
		},
		stdin = true,
		try_node_modules = true,
	}
end

require("formatter").setup({
	filetype = {
		python = { require("formatter.filetypes.python").black },
		rust = { require("formatter.filetypes.rust").rustfmt },
		lua = {
			{
				exe = "stylua",
				args = {
					"--column-width 80",
					"--search-parent-directories",
					"--stdin-filepath",
					util.escape_path(util.get_current_buffer_file_path()),
					"--",
					"-",
				},
				stdin = true,
			},
		},
		go = { require("formatter.filetypes.go").gofmt },
		haskell = { require("formatter.filetypes.haskell").stylish_haskell },
		sh = { require("formatter.filetypes.sh").shfmt },
		c = { clangformat },
		yaml = { require("formatter.filetypes.yaml").prettier },
		json = { require("formatter.filetypes.json").fixjson },
		tex = { require("formatter.filetypes.latex").latexindent },
		typescript = { require("formatter.filetypes.typescript").prettier },
		arduino = { clangformat },
		javascript = { require("formatter.filetypes.javascript").prettier },
		java = { clangformat },
	},
})

local group = vim.api.nvim_create_augroup("FormatterGroup", { clear = true })
vim.api.nvim_create_autocmd(
	"BufWritePost",
	{ command = "FormatWrite", group = group }
)
