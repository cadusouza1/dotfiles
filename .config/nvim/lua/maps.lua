vim.keymap.set("t", "<Esc>", "<C-\\><C-n>") -- Enter normal mode on the terminal
vim.keymap.set("n", "<A-j>", "<cmd>bp<cr>") -- Better buffer navigation
vim.keymap.set("n", "<A-k>", "<cmd>bn<cr>") -- Better buffer navigation
vim.keymap.set("n", "<A-d>", "<cmd>bd<cr>") -- Quickly close a buffer
vim.keymap.set("n", "<A-s>", "<cmd>w<cr>") -- Quickly save a buffer
vim.keymap.set("n", "<A-w>", "<C-w>") -- Better split mapping
vim.keymap.set("n", "<leader>so", "<cmd>so %<cr>") -- Quickly source a file
vim.keymap.set("n", "<Bslash>n", ":cn<cr>") -- Better quick fix list navigation
vim.keymap.set("n", "<Bslash>p", ":cp<cr>") -- Better quick fix list navigation
vim.keymap.set("n", "<leader>cp", '"*p') -- Paste to clipboard in a easier way
vim.keymap.set({ "n", "v" }, "<leader>cy", '"+y') -- Copy to clipboard in a easier way
