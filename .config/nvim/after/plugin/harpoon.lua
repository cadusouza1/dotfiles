local harpoon = require("harpoon")

-- REQUIRED
harpoon:setup()
-- REQUIRED

vim.keymap.set("n", "<leader>ha", function()
	harpoon:list():append()
end)

vim.keymap.set("n", "<C-e>", function()
	harpoon.ui:toggle_quick_menu(harpoon:list())
end)

vim.keymap.set("n", "<C-h>", function()
	harpoon:list():select(1)
end)

vim.keymap.set("n", "<C-j>", function()
	harpoon:list():select(2)
end)

vim.keymap.set("n", "<C-k>", function()
	harpoon:list():select(3)
end)

vim.keymap.set("n", "<C-l>", function()
	harpoon:list():select(4)
end)

-- Toggle previous & next buffers stored within Harpoon list
vim.keymap.set("n", "<C-S-P>", function()
	harpoon:list():prev()
end)

vim.keymap.set("n", "<C-S-N>", function()
	harpoon:list():next()
end)

-- Going even further beyond
vim.keymap.set("n", "<leader>h1", function()
	harpoon:list():select(5)
end)

vim.keymap.set("n", "<leader>h2", function()
	harpoon:list():select(6)
end)

vim.keymap.set("n", "<leader>h3", function()
	harpoon:list():select(7)
end)

vim.keymap.set("n", "<leader>h4", function()
	harpoon:list():select(8)
end)

vim.keymap.set("n", "<leader>h5", function()
	harpoon:list():select(9)
end)
