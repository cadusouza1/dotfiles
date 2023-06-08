local os = require("os")

local packer_command_maps = {
	["i"] = "Install",
	["c"] = "Clean",
	["s"] = "Sync",
}

for key, command in pairs(packer_command_maps) do
	vim.keymap.set(
		"n",
		"<leader>pa" .. key,
		"<cmd>so "
			.. os.getenv("XDG_CONFIG_HOME")
			.. "/nvim/lua/plugins.lua"
			.. "<cr>"
			.. "<cmd>Packer"
			.. command
			.. "<cr>"
	)
end
