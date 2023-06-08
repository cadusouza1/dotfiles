local git_keymaps = {
	["a"] = ":G add %",
	["d"] = ":G diff %",
	["D"] = ":G diff",
	["l"] = ":G log",
	["r"] = ":G rebase -i",
	["s"] = ":G status",
}

for key, map in pairs(git_keymaps) do
	vim.keymap.set("n", "<leader>g" .. key, map .. "<cr>")
end
