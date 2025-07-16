return {
  "nvim-treesitter/nvim-treesitter",
  branch = 'master',
  lazy = false,
  build = ":TSUpdate",
  dependencies = {
    { "HiPhish/rainbow-delimiters.nvim" },
    { "nvim-treesitter/nvim-treesitter-textobjects" }
  },
  config = function() 
        local rainbow_delimiters = require("rainbow-delimiters")

        vim.g.rainbow_delimiters = {
            strategy = {
                [""] = rainbow_delimiters.strategy["global"],
            },
            query = {
                [""] = "rainbow-delimiters",
            },
        }

        require("nvim-treesitter.configs").setup({
            highlight = {
                enable = true,
                extended_mode = true,
            },
            incremental_selection = {
                enable = true,
            },
            indent = {
                enable = true,
            },
            rainbow = {
                enable = true,
                extended_mode = true, -- highlight non-bracket delimiters
                -- query = "rainbow-parens",
                -- strategy = require("ts-rainbow").strategy.global,
            },
            textobjects = {
                select = {
                    enable = true,
                    lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
                    keymaps = {
                        -- ["ib"] = "@block.inner",
                        -- ["ab"] = "@block.outer",
                        -- ["ica"] = "@call.inner",
                        -- ["aca"] = "@call.outer",
                        ["ao"] = "@comment.outer",
                        ["io"] = "@comment.inner",
                        ["if"] = "@function.inner",
                        ["af"] = "@function.outer",
                        ["ir"] = "@loop.inner",
                        ["ar"] = "@loop.outer",
                        ["ia"] = "@parameter.inner",
                        ["aa"] = "@parameter.outer",
                        ["aS"] = "@statement.outer",
                        ["iC"] = "@class.inner",
                        ["aC"] = "@class.outer",
                        ["ic"] = "@conditional.inner",
                        ["ac"] = "@conditional.outer",
                    },

                    -- You can choose the select mode (default is charwise 'v')
                    selection_modes = {
                        ["@parameter.outer"] = "v", -- charwise
                        ["@function.outer"] = "V", -- linewise
                        ["@class.outer"] = "<c-v>", -- blockwise
                    },
                    include_surrounding_whitespace = true,
                },
                swap = {
                    enable = true,
                    swap_next = {
                        ["<leader>af"] = "@function.outer",
                        ["<leader>ar"] = "@loop.outer",
                        ["<leader>aa"] = "@parameter.outer",
                        ["<leader>as"] = "@statement.outer",
                        ["<leader>ac"] = "@conditional.outer",
                        ["<leader>aC"] = "@class.outer",
                    },
                    swap_previous = {
                        ["<leader>Af"] = "@function.outer",
                        ["<leader>Ar"] = "@loop.outer",
                        ["<leader>Aa"] = "@parameter.outer",
                        ["<leader>As"] = "@statement.outer",
                        ["<leader>Ac"] = "@conditional.outer",
                        ["<leader>AC"] = "@class.outer",
                    },
                },
                move = {
                    enable = true,
                    set_jumps = true, -- whether to set jumps in the jumplist
                    goto_next_start = {
                        ["]gf"] = "@function.outer",
                        ["]gr"] = "@loop.outer",
                        ["]ga"] = "@parameter.outer",
                        ["]gs"] = "@statement.outer",
                        -- ["]gcm"] = "@comment.outer",
                        ["]gC"] = "@class.outer",
                        ["]gc"] = "@conditional.outer",
                    },
                    goto_next_end = {
                        ["]Gf"] = "@function.outer",
                        ["]Gr"] = "@loop.outer",
                        ["]Ga"] = "@parameter.outer",
                        ["]Gs"] = "@statement.outer",
                        -- ["]Gcm"] = "@comment.outer",
                        ["]GC"] = "@class.outer",
                        ["]Gc"] = "@conditional.outer",
                    },
                    goto_previous_start = {
                        ["[gf"] = "@function.outer",
                        ["[gr"] = "@loop.outer",
                        ["[ga"] = "@parameter.outer",
                        ["[gs"] = "@statement.outer",
                        -- ["[gcm"] = "@comment.outer",
                        ["[gC"] = "@class.outer",
                        ["[gc"] = "@conditional.outer",
                    },
                    goto_previous_end = {
                        ["[Gf"] = "@function.outer",
                        ["[Gr"] = "@loop.outer",
                        ["[Ga"] = "@parameter.outer",
                        ["[Gs"] = "@statement.outer",
                        -- ["[Gcm"] = "@comment.outer",
                        ["[GC"] = "@class.outer",
                        ["[Gc"] = "@conditional.outer",
                    },
                },
            },

            lsp_interop = {
                enable = true,
                border = "none",
                peek_definiton_code = {
                    ["<leader>pf"] = "@function.outer",
                    ["<leader>pc"] = "@class.outer",
                },
            },
        })
  end
}
