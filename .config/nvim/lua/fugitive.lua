local git_keymaps = {
	["a"] = ":G add %",
	["di"] = ":G diff %",
	["da"] = ":G diff",
	["dp"] = ":Ghdiffsplit",
	["ds"] = ":Gvdiffsplit",
	["dt"] = ":G difftool",
	["ed"] = ":Gedit",
	["c"] = ":G commit",
	["r"] = ":G rebase -i",
	["l"] = ":G log",
	["b"] = ":G blame",
	["m"] = ":G mergetool",
	["s"] = ":G ",
}

for key, map in pairs(git_keymaps) do
	vim.keymap.set("n", "<leader>g" .. key, map .. "<cr>")
end

vim.keymap.set("n", "<leader>gad", ":G add ")
vim.keymap.set("n", "<leader>gpu", ":G push ")
