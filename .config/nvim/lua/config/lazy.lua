local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })

	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})

		vim.fn.getchar()
		os.exit(1)
	end
end

vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.opt.scrolloff = 999
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.relativenumber = true
vim.opt.number = true
vim.opt.clipboard = "unnamedplus"
vim.opt.hlsearch = false -- Remove search highligth
vim.opt.incsearch = true -- incremental search
vim.opt.expandtab = true -- converts tabs to white space
vim.opt.autoindent = true -- indent a new line the same amount as the line just typed
vim.opt.hidden = true
vim.opt.shiftwidth = 4 -- width for autoindents
vim.opt.softtabstop = 4 -- see multiple spaces as tabstops so <BS> does the right thing
vim.opt.tabstop = 4 -- number of columns occupied by a tab
vim.opt.encoding = "utf-8"
-- vim.opt.cmdheight = 2
vim.opt.updatetime = 50
vim.opt.swapfile = false
vim.opt.spelllang = "en,pt_br"

vim.opt.undofile = true
vim.opt.signcolumn = "no"

vim.opt.wrap = true

require("lazy").setup({
	spec = {
		{ import = "plugins" },
	},

	install = { colorscheme = { "gruber-darker" } },
	checker = { enabled = false },
})
