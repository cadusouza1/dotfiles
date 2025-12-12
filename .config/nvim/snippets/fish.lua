return {
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
}
