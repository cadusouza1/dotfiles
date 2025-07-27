return {
	"stevearc/conform.nvim",
	event = { "BufWritePre" },
	cmd = { "ConformInfo" },
	opts = {
		formatters_by_ft = {
			lua = { "stylua" },
			c = { "clang_format" },
			cpp = { "clang_format" },
			python = { "isort", "black" },
		},
		format_on_save = { timeout_ms = 1000 },
		formatters = {
			clang_format = {
				prepend_args = { "--style={IndentWidth: 4}" },
			},
		},
	},
}
