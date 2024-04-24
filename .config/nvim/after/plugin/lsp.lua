local telescope = require("telescope.builtin")
local lsp = require("lsp-zero").preset()
local mason = require("mason")
local mason_lsp = require("mason-lspconfig")

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
vim.keymap.set(
	"n",
	"<Bslash>l",
	telescope.diagnostics,
	{ silent = true }
)

lsp.on_attach(function(_, bufnr)
	vim.api.nvim_buf_set_option(
		bufnr,
		"omnifunc",
		"v:lua.vim.lsp.omnifunc"
	)

	vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "", {
		callback = vim.lsp.buf.hover,
		silent = true,
		noremap = true,
	})
	vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "", {
		callback = telescope.lsp_definition,
		silent = true,
		noremap = true,
	})
	vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "", {
		callback = vim.lsp.buf.declaration,
		silent = true,
		noremap = true,
	})
	vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "", {
		callback = telescope.lsp_implementation,
		silent = true,
		noremap = true,
	})
	vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "", {
		callback = telescope.lsp_references,
		silent = true,
		noremap = true,
	})

	-- vim.api.nvim_buf_set_keymap(
	-- 	bufnr,
	-- 	"n",
	-- 	"<C-h>",
	-- 	"",
	-- 	{ callback = vim.lsp.buf.signature_help, silent = true, noremap = true }
	-- )

	vim.api.nvim_buf_set_keymap(bufnr, "n", "<Bslash>h", "", {
		callback = vim.lsp.buf.code_action,
		silent = true,
		noremap = true,
	})
	vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>rn", "", {
		callback = vim.lsp.buf.rename,
		silent = true,
		noremap = true,
	})

	vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>D", "", {
		callback = telescope.lsp_type_definition,
		silent = true,
		noremap = true,
	})

	vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>wa", "", {
		callback = vim.lsp.buf.add_workspace_folder,
		silent = true,
		noremap = true,
	})

	vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>wr", "", {
		callback = vim.lsp.buf.remove_workspace_folder,
		silent = true,
		noremap = true,
	})

	vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>wl", "", {
		callback = function()
			print(
				vim.inspect(
					vim.lsp.buf.list_workspace_folders()
				)
			)
		end,
		silent = true,
		noremap = true,
	})

	vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>fo", "", {
		callback = function()
			vim.lsp.buf.format({ async = true })
		end,
		silent = true,
		noremap = true,
	})
end)

lsp.setup()
mason.setup()
mason_lsp.setup()
