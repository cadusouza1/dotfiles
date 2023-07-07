local util = require("formatter.util")

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
		javascript = { require("formatter.filetypes.javascript").jsbeatify },
		sh = { require("formatter.filetypes.sh").shfmt },
		c = {
			function()
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
			end,
		},
		yaml = { require("formatter.filetypes.yaml").prettier },
		json = { require("formatter.filetypes.json").fixjson },
		tex = { require("formatter.filetypes.latex").latexindent },
		typescript = { require("formatter.filetypes.typescript").prettier },
		arduino = {
			function()
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
			end,
		},
	},
})

local group = vim.api.nvim_create_augroup("FormatterGroup", { clear = true })
vim.api.nvim_create_autocmd(
	"BufWritePost",
	{ command = "FormatWrite", group = group }
)
