require("packer").startup(function(use)
	use({ "wbthomason/packer.nvim" })
	use({ "mhartington/formatter.nvim" })
	use({ "nvim-lua/plenary.nvim" })

	use({ "smoka7/hop.nvim", tag = "*" })

	use({
		"ThePrimeagen/harpoon",
		branch = "harpoon2",
		requires = { { "nvim-lua/plenary.nvim" } },
	})

	-- colorschemes
	use({
		"jesseleite/nvim-noirbuddy",
		requires = { "tjdevries/colorbuddy.nvim" },
	})

	use({ "Mofiqul/vscode.nvim" })
	use({ "ptdewey/darkearth-nvim" })
	use({ "ellisonleao/gruvbox.nvim" })
	use({ "fcpg/vim-fahrenheit" })
	use({ "f4z3r/gruvbox-material.nvim" })

	use({ "nvim-tree/nvim-web-devicons" })

	-- LSP config
	use({
		"neovim/nvim-lspconfig",
		requires = {
			{ "onsails/lspkind.nvim" },
		},
	})

	use({ "williamboman/mason.nvim" })
	use({ "williamboman/mason-lspconfig.nvim" })

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
			{ "HiPhish/rainbow-delimiters.nvim" },
			{
				"nvim-treesitter/nvim-treesitter-textobjects",
			},
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
	use({ "tpope/vim-vinegar" })

	-- use({ "jiangmiao/auto-pairs" })

	use({ "norcalli/nvim-colorizer.lua" })

	-- More and better vim objects
	use({ "wellle/targets.vim" })
	use({ "bkad/CamelCaseMotion" })

	use({ "lewis6991/impatient.nvim" })

	use({
		"nvim-lualine/lualine.nvim",
		requires = {
			"nvim-tree/nvim-web-devicons",
			opt = true,
		},
	})

	use({ "VonHeikemen/lsp-zero.nvim" })
	use({ "ThePrimeagen/vim-be-good" })
	use({ "kovetskiy/sxhkd-vim" })
end)
