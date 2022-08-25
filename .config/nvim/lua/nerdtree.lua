-- NERDTree
vim.g.NERDTreeCasadeOpenSingleChildDir = 1

vim.keymap.set("n", "<leader>n", ":NERDTreeFocus<CR>")
vim.keymap.set("n", "<C-n>", ":NERDTree<CR>")
vim.keymap.set("n", "<C-t>", ":NERDTreeToggle<CR>")
vim.keymap.set("n", "<C-f>", ":NERDTreeFind<CR>")

-- NERDTree git plugin
vim.g.NERDTreeGitStatusIndicatorMapCustom = {
    ['Modified'] = '✹',
    ['Staged'] = '✚',
    ['Untracked'] = '✭',
    ['Renamed'] = '➜',
    ['Unmerged'] = '═',
    ['Deleted'] = '✖',
    ['Dirty'] = '✗',
    ['Ignored'] = '☒',
    ['Clean'] = '✔︎',
    ['Unknown'] = '?',
}

vim.g.NERDTreeGitStatusUseNerdFonts = 1
vim.g.NERDTreeGitStatusShowClean = 1

-- NERDTree visual selection
vim.g.nerdtree_vis_confirm_open = 1
vim.g.nerdtree_vis_confirm_delete = 1
vim.g.nerdtree_vis_confirm_copy = 1
vim.g.nerdtree_vis_confirm_move = 1
