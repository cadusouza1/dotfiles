require("packer").startup(function(use)
	use({ "wbthomason/packer.nvim" })

	-- colorschemes
	use({ "navarasu/onedark.nvim" })
	use({ "luisiacc/gruvbox-baby" })
	use({ "Mofiqul/dracula.nvim" })
	use({ "fcpg/vim-fahrenheit" })
	use({ "wittyjudge/gruvbox-material.nvim" })

	-- LSP config
	use({
		"neovim/nvim-lspconfig",
		requires = {
			{ "simrat39/symbols-outline.nvim" },
			{ "onsails/lspkind.nvim" },
		},
	})

	use({ "williamboman/mason.nvim" })
	use({ "williamboman/mason-lspconfig.nvim" })

	use({ "mfussenegger/nvim-dap" })

	-- Completion engine and some plugins for it
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			{ "tzachar/cmp-tabnine", run = "./install.sh" },
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "hrsh7th/cmp-buffer" },
			{ "hrsh7th/cmp-path" },
			{ "hrsh7th/cmp-cmdline" },
			{ "hrsh7th/cmp-nvim-lua" },
			{ "saadparwaiz1/cmp_luasnip" },
		},
	})

	-- Snippets
	use({ "L3MON4D3/LuaSnip" })
	use({ "rafamadriz/friendly-snippets" })

	-- Better syntax highlighting
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		requires = {
			{ "HiPhish/nvim-ts-rainbow2" },
			{ "nvim-treesitter/nvim-treesitter-textobjects" },
			{ "nvim-treesitter/nvim-treesitter-context" },
		},
	})

	-- Fuzzy finder
	use({
		"nvim-telescope/telescope.nvim",
		tag = "0.1.0",
		requires = {
			{ "nvim-lua/plenary.nvim" },
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				run = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
			},
		},
	})

	-- Git integration
	use({ "tpope/vim-fugitive" })

	-- Operations to surround text objects
	use({ "tpope/vim-surround" })

	-- Easily comment stuff
	use({ "tpope/vim-commentary" })

	-- Repeat everything with .
	use({ "tpope/vim-repeat" })
	use({ "tpope/vim-unimpaired" })

	use({ "arthurxavierx/vim-caser" })

	use({ "jiangmiao/auto-pairs" })

	use({ "norcalli/nvim-colorizer.lua" })

	use({ "nvim-telescope/telescope-ui-select.nvim" })

	-- More and better vim objects
	use({ "wellle/targets.vim" })
	use({ "bkad/CamelCaseMotion" })
	use({ "vim-scripts/argtextobj.vim" })
	use({ "michaeljsmith/vim-indent-object" })

	use({ "mhartington/formatter.nvim" })

	-- Nvim REPL
	use({ "hkupty/iron.nvim" })

	use({ "s1n7ax/nvim-terminal" })

	use({ "lewis6991/impatient.nvim" })

	use({
		"nvim-lualine/lualine.nvim",
		requires = { "nvim-tree/nvim-web-devicons", opt = true },
	})

	use({ "akinsho/bufferline.nvim", tag = "*", requires = "nvim-tree/nvim-web-devicons" })

	use({
		"iamcco/markdown-preview.nvim",
		run = function()
			vim.fn["mkdp#util#install"]()
		end,
	})

	use({ "klen/nvim-test" })

	use({
		"MrcJkb/haskell-tools.nvim",
		requires = {
			"neovim/nvim-lspconfig",
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",
		},
	})

	use({ "VonHeikemen/lsp-zero.nvim" })

	use({ "ellisonleao/gruvbox.nvim" })
end)

vim.cmd([[
    set termguicolors
    runtime macros/matchit.vim
    colorscheme gruvbox-material
]])

vim.g.AutoPairsShortcutToggle = "<M-z>"
