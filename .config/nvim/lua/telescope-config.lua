local builtin = require("telescope.builtin")
local telescope = require("telescope")
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local themes = require("telescope.themes")

local xdg_config_home = os.getenv("XDG_CONFIG_HOME")
local home = os.getenv("HOME")

local telescope_function_keys = {
	["f"] = builtin.find_files,
	["l"] = builtin.live_grep,
	["g"] = builtin.grep_string,
	["b"] = builtin.buffers,
	["t"] = builtin.tags,
	["m"] = builtin.marks,
	["q"] = builtin.quickfix,
	["j"] = builtin.jumplist,
	["r"] = builtin.registers,
	["s"] = builtin.spell_suggest,
	["h"] = builtin.highlights,
	["c"] = builtin.colorscheme,
}

for key, func in pairs(telescope_function_keys) do
	vim.keymap.set("n", "<leader>f" .. key, func)
end

telescope.setup({
	extensions = {
		fzf = {
			fuzzy = true, -- false will only do exact matching
			override_generic_sorter = true, -- override the generic sorter
			override_file_sorter = true, -- override the file sorter
			case_mode = "smart_case", -- or "ignore_case" or "respect_case"
		},
	},
})

telescope.load_extension("ui-select")
telescope.load_extension("fzf")

local function pdf_picker(search_path)
	local opts = themes.get_dropdown({})
	pickers
		.new(opts, {
			prompt_title = "PDF Finder",
			finder = finders.new_oneshot_job({ "fdfind", "-e", "pdf", "--search-path", search_path }, opts),
			sorter = conf.file_sorter(opts),
			attach_mappings = function(prompt_bufnr, _)
				actions.select_default:replace(function()
					actions.close(prompt_bufnr)
					local selection = action_state.get_selected_entry()
					os.execute("zathura " .. selection[1] .. " & disown")
				end)
				return true
			end,
		})
		:find()
end

vim.keymap.set("n", "<leader>co", function()
	builtin.find_files({
		search_dirs = {
			xdg_config_home,
			home .. "/.xmonad",
			home .. "/.scripts",
		},
	})
end)

vim.keymap.set("n", "<leader>cn", function()
	builtin.find_files({ search_dirs = { xdg_config_home .. "/nvim" } })
end)

vim.keymap.set("n", "<leader>cs", function()
	builtin.find_files({ search_dirs = { home .. "/school" } })
end)

vim.keymap.set("n", "<leader>ch", function()
	builtin.find_files({ search_dirs = { home } })
end)

vim.keymap.set("n", "<leader>cf", function()
	builtin.find_files({ search_dirs = { xdg_config_home .. "/fish/" } })
end)

vim.keymap.set("n", "<leader>Z", function()
	pdf_picker(home)
end)

vim.keymap.set("n", "<leader>z", function()
	pdf_picker(vim.fs.dirname(vim.fn.expand("%")))
end)
