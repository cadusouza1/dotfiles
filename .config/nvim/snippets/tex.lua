return {
	s("bi", { t("\\binom{"), i(1), t("}{"), i(2), t("}"), i(0) }),
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
			local exponent = snip.captures[2]
			return snip.captures[1] .. "^" .. (exponent:len() == 1 and exponent or "{" .. exponent .. "}")
		end, {})
	),
	s("=>", t("\\Rightarrow ")),
	s("<=", t("\\Leftarrow ")),
	s("->", t("\\rightarrow ")),
	s("<-", t("\\leftarrow ")),
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
		t("\\lim_{"),
		i(1),
		t(" \\to "),
		i(2),
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
	s("(", { t("\\left(") }),
	s(")", { t("\\right)") }),
	s("()", { t("\\left( "), i(0), t("\\right") }),
	s("matrix", {
		-- Prompt for M (rows) and N (columns)
		i(1, "M"),
		t("x"),
		i(2, "N"),
		t({ "", "\\begin{bmatrix}" }),
		d(3, function(args)
			local rows = tonumber(args[1][1]) or 2
			local cols = tonumber(args[2][1]) or 2

			-- Create dynamic nodes for matrix elements
			local matrix_nodes = {}
			for row = 1, rows do
				for col = 1, cols do
					table.insert(matrix_nodes, i(row * cols + col, "0"))
					if col < cols then
						table.insert(matrix_nodes, t(" & "))
					end
				end
				if row < rows then
					table.insert(matrix_nodes, t({ " \\\\", "" }))
				end
			end
			return sn(nil, matrix_nodes)
		end, { 1, 2 }),
		t({ "", "\\end{bmatrix}" }),
		i(0),
	}),
	s("eq", {
		i(1),
		t(" \\equiv "),
		i(2),
		t(" \\mod{"),
		i(3),
		t("} "),
		i(0),
	}),
	s("cd", { t({ "\\cdot " }), i(0) }),
}
