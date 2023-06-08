vim.g.mapleader = " "

vim.keymap.set("n", "<leader>ex", vim.cmd.Ex)
vim.keymap.set("n", "<Bslash>n", ":cn<cr>") -- Better quick fix list navigation
vim.keymap.set("n", "<Bslash>p", ":cp<cr>") -- Better quick fix list navigation

vim.keymap.set("t", "<Esc>", "<C-\\><C-n>") -- Enter normal mode on the terminal
vim.keymap.set("n", "<A-w>", "<C-w>") -- Better split mapping (I hate them Control maps)
vim.keymap.set("n", "<leader>so", "<cmd>so %<cr>") -- Quickly source a file
vim.keymap.set("n", "<leader>cp", '"*p') -- Paste to clipboard in a easier way
vim.keymap.set({ "n", "v" }, "<leader>cy", '"+y') -- Copy to clipboard in a easier way
