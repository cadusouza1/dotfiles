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
			html = { "prettier" },
			json = { "prettier" },
		},
		format_on_save = { timeout_ms = 5000 },
		formatters = {
			clang_format = {
				prepend_args = { "--style={IndentWidth: 4}" },
			},
		},
	},
}
