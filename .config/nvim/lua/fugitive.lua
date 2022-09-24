local git_keymaps = {
	["a"] = ":G add %",
	["di"] = ":G diff %",
	["da"] = ":G diff",
	["dp"] = ":Ghdiffsplit",
	["ds"] = ":Gvdiffsplit",
	["dt"] = ":G difftool",
	["ed"] = ":Gedit",
	["sp"] = ":Gsplit",
	["vs"] = ":Gvsplit",
	["co"] = ":G commit",
	["re"] = ":G rebase -i",
	["lo"] = ":G log",
	["bl"] = ":G blame",
	["mt"] = ":G mergetool",
	["st"] = ":G ",
}

for key, map in pairs(git_keymaps) do
	vim.keymap.set("n", "<leader>g" .. key, map .. "<cr>")
end

vim.keymap.set("n", "<leader>gad", ":G add ")
vim.keymap.set("n", "<leader>gpu", ":G push ")
