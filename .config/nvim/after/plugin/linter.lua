require("lint").linters_by_ft = {
	django = { "djlint" },
	go = { "djlint" },
	jinja = { "djlint" },
	html = { "djlint" },
	-- c = { "cpplint" },
	-- cpp = { "cpplint" },
}

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
	callback = function()
		require("lint").try_lint()
	end,
})
