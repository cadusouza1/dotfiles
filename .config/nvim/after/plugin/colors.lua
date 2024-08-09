vim.cmd([[
    set termguicolors
    runtime macros/matchit.vim
    colorscheme gruvbox-material
]])

-- require("base16-colorscheme").setup({
-- 	base00 = "#282828",
-- 	base01 = "#3c3836",
-- 	base02 = "#504945",
-- 	base03 = "#665c54",
-- 	base04 = "#bdae93",
-- 	base05 = "#d5c4a1",
-- 	base06 = "#ebdbb2",
-- 	base07 = "#fbf1c7",
-- 	base08 = "#fb4934",
-- 	base09 = "#fe8019",
-- 	base0A = "#fabd2f",
-- 	base0B = "#b8bb26",
-- 	base0C = "#8ec07c",
-- 	base0D = "#83a598",
-- 	base0E = "#d3869b",
-- 	base0F = "#d65d0e",
-- })

require("colorizer").setup()
vim.api.nvim_create_autocmd({ "BufEnter" }, {
	pattern = "*.md",
	command = "colorscheme gruvbox-material",
	group = vim.api.nvim_create_augroup(
		"MdColorGroup",
		{ clear = true }
	),
})

vim.api.nvim_create_autocmd({ "BufEnter" }, {
	pattern = "*.md",
	command = "set spell",
	group = vim.api.nvim_create_augroup(
		"MdSpellGroup",
		{ clear = true }
	),
})

-- require("noirbuddy").setup({ preset = "minimal" })
