local telescope = require("telescope.builtin")

vim.keymap.set(
    "n",
    "<Bslash>j",
    vim.diagnostic.goto_next,
    { silent = true }
)

vim.keymap.set(
    "n",
    "<Bslash>k",
    vim.diagnostic.goto_prev,
    { silent = true }
)

vim.keymap.set(
    "n",
    "<Bslash>l",
    telescope.diagnostics,
    { silent = true }
)

local lsp_buffer_maps = {
    ["<C-h>"]      = vim.lsp.buf.signature_help,
    ["<leader>fo"] = function() vim.lsp.buf.format { async = true } end,
    ["gd"]         = telescope.lsp_definition,
    ["gD"]         = vim.lsp.buf.declaration,
    ["gi"]         = telescope.lsp_implementation,
    ["gr"]         = telescope.lsp_references,
    ["<Bslash>h"]  = vim.lsp.buf.code_action,
    ["K"]          = vim.lsp.buf.hover,
    ["<leader>D"]  = telescope.lsp_type_definition,
    ["<leader>rn"] = vim.lsp.buf.rename,
    ["<leader>wa"] = vim.lsp.buf.add_workspace_folder,
    ["<leader>wl"] = function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end,
    ["<leader>wr"] = vim.lsp.buf.remove_workspace_folder,
}

---@diagnostic disable-next-line: unused-local
local on_attach = function(client, bufnr)
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

    for key, func in pairs(lsp_buffer_maps) do
        vim.api.nvim_buf_set_keymap(
            bufnr, "n", key, "",
            { callback = func, silent = true, noremap = true }
        )
    end
end

local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())
require("nvim-lsp-installer").on_server_ready(
    function(server)
        local server_opts = { on_attach = on_attach, capabilities = capabilities }

        -- (optional) Customize the options passed to the server
        if server.name == "sumneko_lua" then
            server_opts["settings"] = {
                Lua = {
                    diagnostics = { globals = { "vim" } }
                }
            }
        end

        if server.name == "ltex" then
            server_opts["settings"] = {
                ltex = {
                    language = "en-US"
                }
            }
        end

        -- This setup() function will take the provided server configuration and decorate it with the necessary properties
        -- before passing it onwards to lspconfig.
        -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
        server:setup(server_opts)
    end
)


