require("formatter").setup({
	filetype = {
		python = { require("formatter.filetypes.python").black },
		rust = { require("formatter.filetypes.rust").rustfmt },
		lua = { require("formatter.filetypes.lua").stylua },
        go = { require("formatter.filetypes.go").gofmt }
	},
})

vim.keymap.set("n", "<leader>fo", "<cmd>Format<cr>")

local group = vim.api.nvim_create_augroup("FormatterGroup", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", { command = "FormatWrite", group = group })
