local cmp = require "cmp"
local luasnip = require "luasnip"
local lspkind = require "lspkind"

cmp.setup {
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end
    },
    mapping = {
        ["<C-b>"]      = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
        ["<C-f>"]      = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
        ["<C-leader>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
        ["<C-y>"]      = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
        ["<C-e>"]      = cmp.mapping({ i = cmp.mapping.abort(), c = cmp.mapping.close() }),
        ["<CR>"]       = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<tab>"]      = cmp.mapping.select_next_item(),
        ["<S-tab>"]    = cmp.mapping.select_prev_item()
    },
    sources = cmp.config.sources(
        {
            { name = "luasnip" },
            { name = "nvim_lsp" },
            { name = "cmp_tabnine" },
            { name = "nvim_lua" },
        },
        {
            { name = "buffer" }
        }
    ),
    experimental = {
        native_menu = false,
        ghost_text = true
    },
    formatting = {
        format = lspkind.cmp_format({
            mode = 'symbol', -- show only symbol annotations
            maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)

            -- The function below will be called before any actual modifications from lspkind
            -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
            ---@diagnostic disable-next-line: unused-local
            before = function(entry, vim_item)
                return vim_item
            end
        })
    }
}

-- Set configuration for specific filetype.
cmp.setup.filetype(
    "gitcommit",
    {
        sources = cmp.config.sources(
            {
                { name = "cmp_git" } -- You can specify the `cmp_git` source if you were installed it.
            },
            {
                { name = "buffer" }
            }
        )
    }
)

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(
    "/",
    {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
            { name = "buffer" }
        }
    }
)

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(
    ":",
    {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources(
            {
                { name = "path" }
            },
            {
                { name = "cmdline" }
            }
        )
    }
)
