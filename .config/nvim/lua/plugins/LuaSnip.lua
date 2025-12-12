return {
	"L3MON4D3/LuaSnip",
	version = "v2.*",
	build = "make install_jsregexp",
	lazy = false,
	config = function()
		local ls = require("luasnip")

		require("luasnip.loaders.from_lua").load({ paths = "~/.config/nvim/snippets" })
		require("luasnip.loaders.from_vscode").lazy_load()

		ls.config.set_config({
			history = true,
			updateevents = "TextChanged,TextChangedI",
			enable_autosnippets = true,
		})

		vim.keymap.set({ "i" }, "<A-l>", function()
			ls.expand()
		end, { silent = true })

		vim.keymap.set({ "i", "s" }, "<A-j>", function()
			ls.jump(1)
		end, { silent = true })

		vim.keymap.set({ "i", "s" }, "<A-k>", function()
			ls.jump(-1)
		end, { silent = true })

		vim.keymap.set({ "i", "s" }, "<A-e>", function()
			if ls.choice_active() then
				ls.change_choice(1)
			end
		end, { silent = true })
	end,
}
