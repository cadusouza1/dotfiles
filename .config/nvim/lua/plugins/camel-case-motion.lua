return {
    "bkad/CamelCaseMotion",
    lazy = true,
    opts = {},
    config = function()
        vim.keymap.set(
            "o",
            "i<Space>w",
            "<Plug>CamelCaseMotion_iw",
            { silent = true }
        )
        vim.keymap.set(
            "x",
            "i<Space>w",
            "<Plug>CamelCaseMotion_iw",
            { silent = true }
        )
        vim.keymap.set(
            "o",
            "i<Space>b",
            "<Plug>CamelCaseMotion_ib",
            { silent = true }
        )
        vim.keymap.set(
            "x",
            "i<Space>b",
            "<Plug>CamelCaseMotion_ib",
            { silent = true }
        )
        vim.keymap.set(
            "o",
            "i<Space>e",
            "<Plug>CamelCaseMotion_ie",
            { silent = true }
        )
        vim.keymap.set(
            "x",
            "i<Space>e",
            "<Plug>CamelCaseMotion_ie",
            { silent = true }
        )
        vim.keymap.set(
            "n",
            "<Bslash>w",
            "<Plug>CamelCaseMotion_w",
            { silent = true }
        )
        vim.keymap.set(
            "n",
            "<Bslash>b",
            "<Plug>CamelCaseMotion_b",
            { silent = true }
        )
        vim.keymap.set(
            "n",
            "<Bslash>e",
            "<Plug>CamelCaseMotion_e",
            { silent = true }
        )
        vim.keymap.set(
            "n",
            "<Bslash>g",
            " <Plug>CamelCaseMotion_ge",
            { silent = true }
        )
    end
}
