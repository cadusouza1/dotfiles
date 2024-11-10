local hop = require("hop")
local directions = require("hop.hint").HintDirection

hop.setup({
	keys = "etovxqpdygfblzhckisuran",
})

vim.keymap.set("n", "<Bslash>f", function()
	hop.hint_char1({
		direction = directions.AFTER_CURSOR,
		current_line_only = true,
	})
end)

vim.keymap.set("", "<Bslash>F", function()
	hop.hint_char1({
		direction = directions.BEFORE_CURSOR,
		current_line_only = true,
	})
end)

vim.keymap.set("", "<Bslash>t", function()
	hop.hint_char1({
		direction = directions.AFTER_CURSOR,
		current_line_only = true,
		hint_offset = -1,
	})
end)

vim.keymap.set("", "<Bslash>T", function()
	hop.hint_char1({
		direction = directions.BEFORE_CURSOR,
		current_line_only = true,
		hint_offset = 1,
	})
end)

vim.keymap.set("n", "<Bslash>w", function()
	hop.hint_words({
		direction = directions.AFTER_CURSOR,
		current_line_only = true,
	})
end)

vim.keymap.set("", "<Bslash>W", function()
	hop.hint_words({
		direction = directions.BEFORE_CURSOR,
		current_line_only = true,
	})
end)
