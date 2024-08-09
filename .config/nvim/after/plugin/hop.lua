-- place this in one of your configuration file(s)
local hop = require("hop")
local directions = require("hop.hint").HintDirection

hop.setup()
vim.keymap.set("", "<Bslash>f", function()
	hop.hint_char1({
		direction = directions.AFTER_CURSOR,
		current_line_only = true,
	})
end, { remap = true })

vim.keymap.set("", "<Bslash>F", function()
	hop.hint_char1({
		direction = directions.BEFORE_CURSOR,
		current_line_only = true,
	})
end, { remap = true })

vim.keymap.set("", "<Bslash>t", function()
	hop.hint_char1({
		direction = directions.AFTER_CURSOR,
		current_line_only = true,
		hint_offset = -1,
	})
end, { remap = true })

vim.keymap.set("", "<Bslash>T", function()
	hop.hint_char1({
		direction = directions.BEFORE_CURSOR,
		current_line_only = true,
		hint_offset = 1,
	})
end, { remap = true })
