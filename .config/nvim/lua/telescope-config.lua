local telescope = require("telescope.builtin")

local telescope_function_keys = {
    ["f"] = telescope.find_files,
    ["l"] = telescope.live_grep,
    ["g"] = telescope.grep_string,
    ["b"] = telescope.buffers,
    ["t"] = telescope.tags,
    ["m"] = telescope.marks,
    ["q"] = telescope.quickfix,
    ["j"] = telescope.jumplist,
    ["r"] = telescope.registers,
    ["s"] = telescope.spell_suggest,
    ["h"] = telescope.highlights,
    ["c"] = telescope.colorscheme,
}

for key, func in pairs(telescope_function_keys) do
    vim.keymap.set("n", "<leader>f" .. key, func)
end

require("telescope").setup {
    extensions = {
        fzf = {
            fuzzy = true, -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = "smart_case", -- or "ignore_case" or "respect_case"
        }
    }
}

require("telescope").load_extension("ui-select")
require("telescope").load_extension("fzf")
