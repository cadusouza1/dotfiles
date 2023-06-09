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
		query = "rainbow-parens",
		strategy = require("ts-rainbow").strategy.global,
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

-- Defaults
require("treesitter-context").setup({
	enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
	max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
	trim_scope = "outer", -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
	min_window_height = 0, -- Minimum editor window height to enable context. Values <= 0 mean no limit.
	patterns = { -- Match patterns for TS nodes. These get wrapped to match at word boundaries.
		-- For all filetypes
		-- Note that setting an entry here replaces all other patterns for this entry.
		-- By setting the 'default' entry below, you can control which nodes you want to
		-- appear in the context window.
		default = {
			"class",
			"function",
			"method",
			"for",
			"while",
			"if",
			"switch",
			"case",
		},
		-- Patterns for specific filetypes
		-- If a pattern is missing, *open a PR* so everyone can benefit.
		tex = {
			"chapter",
			"section",
			"subsection",
			"subsubsection",
		},
		rust = {
			"impl_item",
			"struct",
			"enum",
		},
		scala = {
			"object_definition",
		},
		vhdl = {
			"process_statement",
			"architecture_body",
			"entity_declaration",
		},
		markdown = {
			"section",
		},
		elixir = {
			"anonymous_function",
			"arguments",
			"block",
			"do_block",
			"list",
			"map",
			"tuple",
			"quoted_content",
		},
		json = {
			"pair",
		},
		yaml = {
			"block_mapping_pair",
		},
	},
	exact_patterns = {
		-- Example for a specific filetype with Lua patterns
		-- Treat patterns.rust as a Lua pattern (i.e "^impl_item$" will
		-- exactly match "impl_item" only)
		-- rust = true,
	},

	-- [!] The options below are exposed but shouldn't require your attention,
	--     you can safely ignore them.

	zindex = 20, -- The Z-index of the context window
	mode = "cursor", -- Line used to calculate context. Choices: 'cursor', 'topline'
	-- Separator between context and content. Should be a single character string, like '-'.
	-- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
	separator = nil,
})
