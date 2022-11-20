local telescope = require("telescope.builtin")
local lspconfig = require("lspconfig")
local mason = require("mason")
local mason_lspconfig = require("mason-lspconfig")

vim.keymap.set("n", "<Bslash>j", vim.diagnostic.goto_next, { silent = true })

vim.keymap.set("n", "<Bslash>k", vim.diagnostic.goto_prev, { silent = true })

vim.keymap.set("n", "<Bslash>l", telescope.diagnostics, { silent = true })

local lsp_buffer_maps = {
	["K"] = vim.lsp.buf.hover,
	["gd"] = telescope.lsp_definition,
	["gD"] = vim.lsp.buf.declaration,
	["gi"] = telescope.lsp_implementation,
	["gr"] = telescope.lsp_references,
	["<C-h>"] = vim.lsp.buf.signature_help,
	["<Bslash>h"] = vim.lsp.buf.code_action,
	["<leader>D"] = telescope.lsp_type_definition,
	["<leader>rn"] = vim.lsp.buf.rename,
	["<leader>wa"] = vim.lsp.buf.add_workspace_folder,
	["<leader>wr"] = vim.lsp.buf.remove_workspace_folder,
	["<leader>wl"] = function()
		print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
	end,
	["<leader>fo"] = function()
		vim.lsp.buf.format({ async = true })
	end,
}

---@diagnostic disable-next-line: unused-local
local on_attach = function(client, bufnr)
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

	for key, func in pairs(lsp_buffer_maps) do
		vim.api.nvim_buf_set_keymap(bufnr, "n", key, "", { callback = func, silent = true, noremap = true })
	end
end

local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

lspconfig.jsonls.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

lspconfig.sumneko_lua.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

lspconfig.bashls.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

lspconfig.pyright.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

lspconfig.rust_analyzer.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

lspconfig.texlab.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

lspconfig.clangd.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

lspconfig.hls.setup({
	capabilities = capabilities,
	on_attach = on_attach,
	cmd = { "haskell-language-server-wrapper", "--lsp" },
})

lspconfig.gopls.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

lspconfig.tsserver.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

mason.setup()
mason_lspconfig.setup()
