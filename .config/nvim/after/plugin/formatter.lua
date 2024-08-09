local util = require("formatter.util")

local clangformat = function()
	return {
		exe = "clang-format",
		args = {
			"-style={IndentWidth: 4}",
			"-assume-filename",
			util.escape_path(
				util.get_current_buffer_file_name()
			),
		},
		stdin = true,
		try_node_modules = true,
	}
end

local black = function()
	return {
		exe = "black",
		args = {
			"-q",
			"-l 80",
			"--stdin-filename",
			util.escape_path(
				util.get_current_buffer_file_name()
			),
			"-",
		},
		stdin = true,
	}
end

local stylua = function()
	return {
		exe = "stylua",
		args = {
			"--column-width 60",
			"--search-parent-directories",
			"--stdin-filepath",
			util.escape_path(
				util.get_current_buffer_file_path()
			),
			"--",
			"-",
		},
		stdin = true,
	}
end

local prettier = function(parser)
	if not parser then
		return {
			exe = "prettier",
			args = {
				"--stdin-filepath",
				"--tab-width 4",
				util.escape_path(
					util.get_current_buffer_file_path()
				),
			},
			stdin = true,
			try_node_modules = true,
		}
	end

	return {
		exe = "prettier",
		args = {
			"--stdin-filepath",
			util.escape_path(
				util.get_current_buffer_file_path()
			),
			"--parser",
			parser,
		},
		stdin = true,
		try_node_modules = true,
	}
end

require("formatter").setup({
	filetype = {
		python = {
			black,
			require("formatter.filetypes.python").isort,
		},
		lua = { stylua },
		c = { clangformat },
		arduino = { clangformat },
		java = {
			require("formatter.filetypes.java").google_java_format,
		},
		rust = {
			require("formatter.filetypes.rust").rustfmt,
		},
		go = { require("formatter.filetypes.go").gofmt },
		haskell = {
			require("formatter.filetypes.haskell").stylish_haskell,
		},
		sh = { require("formatter.filetypes.sh").shfmt },
		json = {
			require("formatter.filetypes.json").fixjson,
		},
		tex = {
			require("formatter.filetypes.latex").latexindent,
		},
		yaml = { prettier },
		typescript = { prettier },
		typescriptreact = { prettier },
		javascript = { prettier },
		html = { prettier },
		sql = {
			require("formatter.filetypes.sql").pgformat,
		},
	},
})

vim.api.nvim_create_autocmd("BufWritePost", {
	command = "FormatWrite",
	group = vim.api.nvim_create_augroup(
		"FormatterGroup",
		{ clear = true }
	),
})
