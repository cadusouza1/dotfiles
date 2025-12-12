return {
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
				return snip.captures[1] .. "^" .. snip.captures[2]
			else
				return snip.captures[1] .. "^" .. "{" .. snip.captures[2] .. "}"
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
	--s({ trig = "tb([0-9])x([0-9])", regTrig = true }, {
	--	-- Prototype
	--	-- This probably does not work
	--	f(function(args, snip, user_args)
	--		---@diagnostic disable-next-line: redefined-local
	--		for i = 1, snip.captures[1] do
	--			for j = 1, snip.captures[2] do
	--				t("&")
	--				i(i + j, { "a_{" .. i .. j .. "}" })
	--				t("& \\\\")
	--			end
	--		end
	--	end),
	--}),
	s("(", { t("\\left(") }),
	s(")", { t("\\right)") }),
	s("()", { t("\\left( "), i(0), t("\\right") }),
	s("cd", { t({ "\\cdot " }), i(0) }),
}
