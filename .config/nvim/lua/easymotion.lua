local easymotion_prefix = "<Bslash><Bslash>"
vim.keymap.set("n", easymotion_prefix, "<Plug>(easymotion-prefix)")

local easymotion_maps = {
    ["s"]  = "<Plug>(easymotion-s)",
    ["f"]  = "<Plug>(easymotion-overwin-f)",
    ["mf"] = "<Plug>(easymotion-sn)",
    ["g"]  = "<Plug>(easymotion-overwin-f2)",
    ["w"]  = "<Plug>(easymotion-overwin-w)",
    ["t"]  = "<Plug>(easymotion-bd-t)",
    -- ["T"]  = "<Plug>(easymotion-T)",
    ["W"]  = "<Plug>(easymotion-W)",
    ["b"]  = "<Plug>(easymotion-bd-w)",
    ["B"]  = "<Plug>(easymotion-bd-W)",
    ["e"]  = "<Plug>(easymotion-bd-e)",
    ["E"]  = "<Plug>(easymotion-bd-E)",
    -- ["ge"] = "<Plug>(easymotion-ge)",
    -- ["gE"] = "<Plug>(easymotion-gE)",
    ["j"]  = "<Plug>(easymotion-bd-jk)",
    -- ["k"]  = "<Plug>(easymotion-k)",
    ["n"]  = "<Plug>(easymotion-bd-n)",
    -- ["N"]  = "<Plug>(easymotion-N)",
    ["."]  = "<Plug>(easymotion-repeat)",
    ["l"]  = "<Plug>(easymotion-overwin-line)",
    -- ["k"]  = "<Plug>(easymotion-next)",
    -- ["p"]  = "<Plug>(easymotion-prev)"
}

for key, map in pairs(easymotion_maps) do
    vim.keymap.set("n", easymotion_prefix .. key, map)
end
