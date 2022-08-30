require("packer").startup(
    function(use)
        use { "wbthomason/packer.nvim" }

        -- colorschemes
        use { "navarasu/onedark.nvim" }
        use { "mjlaufer/gruvbox-darker.nvim" }
        use { "luisiacc/gruvbox-baby" }
        use { "catppuccin/nvim", as = "catppuccin" }
        use { "Mofiqul/dracula.nvim" }
        use { "sainnhe/everforest" }
        use { "shaunsingh/nord.nvim" }
        use { "Mofiqul/vscode.nvim" }
        use { "shaunsingh/oxocarbon.nvim", run = "./install.sh" }

        -- LSP config
        use { "neovim/nvim-lspconfig",
            requires = {
                { "simrat39/symbols-outline.nvim" },
                { "onsails/lspkind.nvim" }
            }
        }

        use { "williamboman/mason.nvim" }
        use { "williamboman/mason-lspconfig.nvim" }

        use { "mfussenegger/nvim-dap" }

        -- Completion engine and some plugins for it
        use { "hrsh7th/nvim-cmp",
            requires = {
                { "tzachar/cmp-tabnine", run = "./install.sh" },
                { "hrsh7th/cmp-nvim-lsp" },
                { "hrsh7th/cmp-buffer" },
                { "hrsh7th/cmp-path" },
                { "hrsh7th/cmp-cmdline" },
                { "hrsh7th/cmp-nvim-lua" },
                { "saadparwaiz1/cmp_luasnip" }
            }
        }

        -- Snippets
        use { "L3MON4D3/LuaSnip" }
        use { "rafamadriz/friendly-snippets" }

        -- Better syntax highlighting
        use { "nvim-treesitter/nvim-treesitter",
            run = ":TSUpdate",
            requires = {
                { "p00f/nvim-ts-rainbow" },
                { "nvim-treesitter/nvim-treesitter-textobjects" }
            }
        }

        -- Fuzzy finder
        use {
            "nvim-telescope/telescope.nvim", tag = "0.1.0",
            requires = {
                { "nvim-lua/plenary.nvim" },
                { "nvim-telescope/telescope-fzf-native.nvim",
                    run = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build" }
            }
        }

        -- Bufferline
        use { "akinsho/bufferline.nvim", disable = true,
            requires = {
                "kyazdani42/nvim-web-devicons"
            }
        }

        -- Git integration
        use { "tpope/vim-fugitive" }

        -- Operations with surrounding objects
        use { "tpope/vim-surround" }

        -- Easily comment stuff
        use { "tpope/vim-commentary" }

        -- Repeat everything with .
        use { "tpope/vim-repeat" }

        use { "tpope/vim-unimpaired" }

        use { "arthurxavierx/vim-caser" }

        use { "jiangmiao/auto-pairs" }

        -- Better way to move around
        use { "easymotion/vim-easymotion",
            requires = {
                { "tpope/vim-repeat" }
            }
        }

        use { "norcalli/nvim-colorizer.lua" }

        use { "nvim-telescope/telescope-ui-select.nvim" }

        -- More and better vim objects
        use { "wellle/targets.vim" }
        use { "bkad/CamelCaseMotion" }
        use { "vim-scripts/argtextobj.vim" }
        use { "michaeljsmith/vim-indent-object" }

        -- Better integration with sxhkd config files
        use { "baskerville/vim-sxhkdrc" }
        use { "kovetskiy/sxhkd-vim" }
    end
)

vim.o.background = "dark"
vim.g.catppuccin_flavour = "mocha"
vim.g.everforest_background = "hard"
vim.g.everforest_better_performance = 1

vim.cmd([[
    set termguicolors
    colorscheme gruvbox-baby
]])

vim.cmd("runtime macros/matchit.vim")

vim.g.AutoPairsShortcutToggle = "<M-z>"
