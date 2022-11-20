require("formatter").setup({
	filetype = {
		python = { require("formatter.filetypes.python").black },
		rust = { require("formatter.filetypes.rust").rustfmt },
		lua = { require("formatter.filetypes.lua").stylua },
		go = { require("formatter.filetypes.go").gofmt },
		haskell = { require("formatter.filetypes.haskell").stylish_haskell },
		javascript = { require("formatter.filetypes.javascript").jsbeatify },
		sh = { require("formatter.filetypes.sh").shfmt },
		c = { require("formatter.filetypes.c").clangformat },
		yaml = { require("formatter.filetypes.yaml").prettier },
	},
})

local group = vim.api.nvim_create_augroup("FormatterGroup", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", { command = "FormatWrite", group = group })
