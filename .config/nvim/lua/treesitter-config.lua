require("nvim-treesitter.configs").setup({
	highlight = {
		enable = true,
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
	},
	textobjects = {
		select = {
			enable = true,

			-- Automatically jump forward to textobj, similar to targets.vim
			lookahead = true,

			keymaps = {
				["ib"] = "@block.inner",
				["ab"] = "@block.outer",
				["if"] = "@function.inner",
				["af"] = "@function.outer",
				["ir"] = "@loop.inner",
				["ar"] = "@loop.outer",
				["ia"] = "@parameter.inner",
				["aa"] = "@parameter.outer",
				["aS"] = "@statement.outer",
				["ica"] = "@call.inner",
				["aca"] = "@call.outer",
				["icl"] = "@class.inner",
				["acl"] = "@class.outer",
				["acm"] = "@comment.outer",
				["ico"] = "@conditional.inner",
				["aco"] = "@conditional.outer",
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
				["<leader>acl"] = "@class.outer",
				["<leader>aco"] = "@conditional.outer",
			},
			swap_previous = {
				["<leader>Af"] = "@function.outer",
				["<leader>Ar"] = "@loop.outer",
				["<leader>Aa"] = "@parameter.outer",
				["<leader>As"] = "@statement.outer",
				["<leader>Acl"] = "@class.outer",
				["<leader>Aco"] = "@conditional.outer",
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
				["]gcm"] = "@comment.outer",
				["]gcl"] = "@class.outer",
				["]gco"] = "@conditional.outer",
			},
			goto_next_end = {
				["]Gf"] = "@function.outer",
				["]Gr"] = "@loop.outer",
				["]Ga"] = "@parameter.outer",
				["]Gs"] = "@statement.outer",
				["]Gcm"] = "@comment.outer",
				["]Gcl"] = "@class.outer",
				["]Gco"] = "@conditional.outer",
			},
			goto_previous_start = {
				["[gf"] = "@function.outer",
				["[gr"] = "@loop.outer",
				["[ga"] = "@parameter.outer",
				["[gs"] = "@statement.outer",
				["[gcm"] = "@comment.outer",
				["[gcl"] = "@class.outer",
				["[gco"] = "@conditional.outer",
			},
			goto_previous_end = {
				["[Gf"] = "@function.outer",
				["[Gr"] = "@loop.outer",
				["[Ga"] = "@parameter.outer",
				["[Gs"] = "@statement.outer",
				["[Gcm"] = "@comment.outer",
				["[Gcl"] = "@class.outer",
				["[Gco"] = "@conditional.outer",
			},
		},
	},
})
