local utils = {}
function utils.set_buffer_and_split_keymaps(key, map_value)
    vim.keymap.set("n", "<leader>co" .. key, "<cmd>e " .. map_value .. "<cr>")
    vim.keymap.set("n", "<leader>sp" .. key, "<cmd>sp " .. map_value .. "<cr>")
    vim.keymap.set("n", "<leader>vs" .. key, "<cmd>vs " .. map_value .. "<cr>")
end

function utils.set_keymaps_with_prefix(key_prefix, map_prefix, keymaps)
    for key, map_value in pairs(keymaps) do
        vim.keymap.set("n", key_prefix .. key, map_prefix .. map_value)
    end
end

return utils
