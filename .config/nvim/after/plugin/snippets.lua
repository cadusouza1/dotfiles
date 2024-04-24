---@diagnostic disable: unused-local
-- vim.g.UltiSnipsExpandTrigger = "<tab>"
-- vim.g.UltiSnipsJumpForwardTrigger = "<c-j>"
-- vim.g.UltiSnipsJumpBackwardTrigger = "<c-k>"
-- vim.g.UltiSnipsEditSplit = "vertical"

-- vim.g.python_host_prog = "/usr/bin/python"
-- vim.g.python3_host_prog = "/usr/bin/python3"

-- vim.keymap.set("n", "<leader>ue", "<cmd>UltiSnipsEdit<cr>")

require("luasnip.loaders.from_vscode").lazy_load()

local ls = require("luasnip")
local c = ls.choice_node
local d = ls.dynamic_node
local f = ls.function_node
local i = ls.insert_node
local m = require("luasnip.extras").m
local r = ls.restore_node
local s = ls.snippet
local t = ls.text_node
local ai = require("luasnip.nodes.absolute_indexer")
local sn = ls.snippet_node
local fmt = require("luasnip.extras.fmt").fmt
local isn = ls.indent_snippet_node
local types = require("luasnip.util.types")
local events = require("luasnip.util.events")
local lambda = require("luasnip.extras").l
local postfix = require("luasnip.extras.postfix").postfix

ls.config.set_config({
	history = true,
	updateevents = "TextChanged,TextChangedI",
	enable_autosnippets = true,
})

vim.keymap.set({ "i", "s" }, "<A-j>", function()
	if ls.expand_or_jumpable() then
		ls.expand_or_jump()
	end
end, { silent = true })

vim.keymap.set({ "i", "s" }, "<A-k>", function()
	if ls.jumpable(-1) then
		ls.jump(-1)
	end
end, { silent = true })

vim.keymap.set({ "i" }, "<A-l>", function()
	if ls.choice_active() then
		ls.change_choice(1)
	end
end)

ls.add_snippets("tex", {
	s("flan", {
		t({ "\\begin{flalign*}", "\t& " }),
		i(1),
		t({ " &\\\\", "\\end{flalign*}", "" }),
		i(0),
	}),
	s("iflan", {
		t({ "\\item", "\t\\begin{flalign*}", "\t\t& " }),
		i(1),
		t({ " &\\\\", "\t\\end{flalign*}", "", "" }),
		i(0),
	}),
	s(
		{ trig = "v([a-zA-Z]+)", regTrig = true },
		f(function(args, snip, user_args)
			return "\\vec{" .. snip.captures[1] .. "}"
		end, {})
	),
	s("frac", {
		t("\\frac{ "),
		i(1),
		t(" }"),
		t("{ "),
		i(2),
		t(" } "),
		i(0),
	}),
	s("&", {
		t("& "),
		i(1),
		t(" &\\\\"),
		i(0),
	}),
	s(
		{ trig = "([a-zA-Z]+)(%d+)", regTrig = true },
		f(function(args, snip, user_args)
			if snip.captures[2]:len() == 1 then
				return snip.captures[1]
					.. "^"
					.. snip.captures[2]
			else
				return snip.captures[1]
					.. "^"
					.. "{"
					.. snip.captures[2]
					.. "}"
			end
		end, {})
	),
	s("Ra", t("\\Rightarrow ")),
	s("La", t("\\Leftarrow ")),
	s("ra", t("\\rightarrow ")),
	s("la", t("\\leftarrow ")),
	s("xx", t("\\times ")),
	s("br", {
		t("\\bigg\\rvert_{"),
		i(1),
		t("}^{"),
		i(2),
		t("}"),
		i(0),
	}),
	s("fflan", {
		t({
			"\\item \\leavevmode\\vadjust{\\vspace{-\\baselineskip}}\\newline",
			"\t\\begin{tikzpicture}",
			"\t\t\\begin{axis}[",
			"\t\t\tsmooth,",
			"\t\t\taxis lines = middle",
			"\t\t]",
			"\t\t\t",
		}),
		i(1),
		t({
			"",
			"\t\t\\end{axis}",
			"",
			"\t\\end{tikzpicture}",
			"",
			"\\begin{flalign*}",
			"\t& ",
		}),
		i(2),
		t({ " &\\\\", "\\end{flalign*}", "" }),
		i(0),
	}),
	s("&&", {
		t("& "),
		i(1),
		t(" & & "),
		i(2),
		t(" &\\\\"),
		i(0),
	}),
	s("lim", {
		t("\\lim_{x \\to "),
		i(1),
		t("} "),
		i(0),
	}),
	s("limh", {
		t("\\lim_{h \\to 0} \\frac{ "),
		i(1),
		t(" }{ h }"),
		i(0),
	}),
	s("lima", {
		t("\\lim_{x \\to "),
		i(1),
		t("} \\frac{ "),
		i(2),
		t(" }{ x "),
		i(3),
		t(" } "),
		i(0),
	}),
	s("prod", {
		t("\\displaystyle \\prod_{ "),
		i(1),
		t(" }^{ "),
		i(2),
		t(" } "),
		i(0),
	}),
	s({ trig = "tb([0-9])x([0-9])", regTrig = true }, {
		-- Prototype
		-- This probably does not work
		f(function(args, snip, user_args)
			---@diagnostic disable-next-line: redefined-local
			for i = 1, snip.captures[1] do
				for j = 1, snip.captures[2] do
					t("&")
					i(i + j, { "a_{" .. i .. j .. "}" })
					t("& \\\\")
				end
			end
		end),
	}),
	s("(", { t("\\left(") }),
	s(")", { t("\\right)") }),
	s("()", { t("\\left( "), i(0), t("\\right") }),
})

ls.add_snippets("fish", {
	s("fun", {
		t("function "),
		i(1),
		t({ "", "\t" }),
		i(2),
		t({ "", "end", "" }),
		i(0),
	}),
	s("for", {
		t("for "),
		i(1),
		t(" in "),
		i(2),
		t({ "", "\t" }),
		i(3),
		t({ "", "end", "" }),
		i(0),
	}),
})

ls.add_snippets("markdown", {
	s("flan", {
		t({ "\\begin{flalign*}", "\t& " }),
		i(1),
		t({ " &\\\\", "\\end{flalign*}", "" }),
		i(0),
	}),
	s("iflan", {
		t({ "\\item", "\t\\begin{flalign*}", "\t\t& " }),
		i(1),
		t({ " &\\\\", "\t\\end{flalign*}", "", "" }),
		i(0),
	}),
	s(
		{ trig = "v([a-zA-Z]+)", regTrig = true },
		f(function(args, snip, user_args)
			return "\\vec{" .. snip.captures[1] .. "}"
		end, {})
	),
	s("frac", {
		t("\\frac{ "),
		i(1),
		t(" }"),
		t("{ "),
		i(2),
		t(" } "),
		i(0),
	}),
	s("&", {
		t("& "),
		i(1),
		t(" &\\\\"),
		i(0),
	}),
	s(
		{ trig = "([a-zA-Z]+)(%d+)", regTrig = true },
		f(function(args, snip, user_args)
			if snip.captures[2]:len() == 1 then
				return snip.captures[1]
					.. "^"
					.. snip.captures[2]
			else
				return snip.captures[1]
					.. "^"
					.. "{"
					.. snip.captures[2]
					.. "}"
			end
		end, {})
	),
	s("Ra", t("\\Rightarrow ")),
	s("La", t("\\Leftarrow ")),
	s("ra", t("\\rightarrow ")),
	s("la", t("\\leftarrow ")),
	s("xx", t("\\times ")),
	s("br", {
		t("\\bigg\\rvert_{"),
		i(1),
		t("}^{"),
		i(2),
		t("}"),
		i(0),
	}),
	s("fflan", {
		t({
			"\\item \\leavevmode\\vadjust{\\vspace{-\\baselineskip}}\\newline",
			"\t\\begin{tikzpicture}",
			"\t\t\\begin{axis}[",
			"\t\t\tsmooth,",
			"\t\t\taxis lines = middle",
			"\t\t]",
			"\t\t\t",
		}),
		i(1),
		t({
			"",
			"\t\t\\end{axis}",
			"",
			"\t\\end{tikzpicture}",
			"",
			"\\begin{flalign*}",
			"\t& ",
		}),
		i(2),
		t({ " &\\\\", "\\end{flalign*}", "" }),
		i(0),
	}),
	s("&&", {
		t("& "),
		i(1),
		t(" & & "),
		i(2),
		t(" &\\\\"),
		i(0),
	}),
	s("lim", {
		t("\\lim_{x \\to "),
		i(1),
		t("} "),
		i(0),
	}),
	s("limh", {
		t("\\lim_{h \\to 0} \\frac{ "),
		i(1),
		t(" }{ h }"),
		i(0),
	}),
	s("lima", {
		t("\\lim_{x \\to "),
		i(1),
		t("} \\frac{ "),
		i(2),
		t(" }{ x "),
		i(3),
		t(" } "),
		i(0),
	}),
	s("prod", {
		t("\\displaystyle \\prod_{ "),
		i(1),
		t(" }^{ "),
		i(2),
		t(" } "),
		i(0),
	}),
	s({ trig = "tb([0-9])x([0-9])", regTrig = true }, {
		-- Prototype
		-- This probably does not work
		f(function(args, snip, user_args)
			---@diagnostic disable-next-line: redefined-local
			for i = 1, snip.captures[1] do
				for j = 1, snip.captures[2] do
					t("&")
					i(i + j, { "a_{" .. i .. j .. "}" })
					t("& \\\\")
				end
			end
		end),
	}),
	s("(", { t("\\left(") }),
	s(")", { t("\\right)") }),
	s("()", { t("\\left( "), i(0), t("\\right") }),
})
