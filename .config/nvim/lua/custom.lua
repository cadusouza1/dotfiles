local os = require "os"

-- Options --
vim.g.mapleader        = " "
vim.opt.scrolloff      = 999
vim.opt.splitright     = true
vim.opt.splitbelow     = true
vim.opt.relativenumber = true
vim.opt.number         = true
vim.opt.clipboard      = "unnamedplus"
vim.opt.hlsearch       = false -- Remove search highligth
vim.opt.incsearch      = true -- incremental search
vim.opt.expandtab      = true -- converts tabs to white space
vim.opt.autoindent     = true -- indent a new line the same amount as the line just typed
vim.opt.hidden         = true
vim.opt.shiftwidth     = 4 -- width for autoindents
vim.opt.softtabstop    = 4 -- see multiple spaces as tabstops so <BS> does the right thing
vim.opt.tabstop        = 4 -- number of columns occupied by a tab
vim.opt.encoding       = "utf-8"

local function set_buffer_and_split_keymap(key, map)
    vim.keymap.set("n", "<leader>co" .. key, "<cmd>e " .. map .. "<cr>")
    vim.keymap.set("n", "<leader>sp" .. key, "<cmd>sp " .. map .. "<cr>")
    vim.keymap.set("n", "<leader>vs" .. key, "<cmd>vs " .. map .. "<cr>")
end

-- Mappings --
local edit_locations = {
    [os.getenv("XDG_CONFIG_HOME")] = {
        ["fish"] = {
            ["fi"] = "config.fish"
        },
        ["picom"] = {
            ["pi"] = "picom.conf"
        },
        ["nvim/lua"] = {
            ["co"] = "completion.lua",
            ["cu"] = "custom.lua",
            ["in"] = "../init.lua",
            ["la"] = "latex-preview.lua",
            ["pa"] = "pairs.lua",
            ["pl"] = "plugins.lua",
            ["sn"] = "snippets.lua",
            ["te"] = "telescope-config.lua",
            ["tr"] = "treesitter-config.lua",
            ["pk"] = "pk.lua",
            ["ls"] = "lsp.lua",
            ["fu"] = "fugitive.lua",
            ["ne"] = "nerdtree.lua",
            ["ea"] = "easymotion.lua",
            ["au"] = "autorun.lua",
            ["sy"] = "symbols-outline-config.lua",
        }
    },
    [os.getenv("HOME")] = {
        ["school"] = {
            ["ad"] = "adm",
            ["al"] = "algebra-linear",
            ["ar"] = "arquitetura",
            ["ca"] = "calculo",
            ["lo"] = "logica",
            ["pa"] = "pac",
        },
        [".xmonad"] = {
            ["xm"] = "xmonad.hs"
        }
    },
}

for base_dir, dirs in pairs(edit_locations) do
    for dir, locations in pairs(dirs) do
        for key, location in pairs(locations) do
            set_buffer_and_split_keymap(key, base_dir .. "/" .. dir .. "/" .. location)
        end
    end
end

vim.keymap.set("n", "<leader>zL", function()
    os.execute("zathura " .. vim.fs.dirname(vim.fn.getreg("%")) .. "/Lista*.pdf &")
end)

vim.keymap.set("n", "<leader>zl", function()
    os.execute("zathura " .. vim.fs.dirname(vim.fn.getreg("%")) .. "/lista*.pdf &")
end)
-- vim.keymap.set("n", "<A-q>", "<cmd>wq<cr>") -- Faster way to save and quit
vim.keymap.set("n", "<A-j>", "<cmd>bp<cr>") -- Better buffer navigation
vim.keymap.set("n", "<A-k>", "<cmd>bn<cr>") -- Better buffer navigation
vim.keymap.set("n", "<A-d>", "<cmd>bd<cr>") -- Quickly close a buffer
vim.keymap.set("n", "<A-s>", "<cmd>w<cr>") -- Quickly save a buffer
vim.keymap.set("n", "<A-w>", "<C-w>") -- Better split mapping
vim.keymap.set("n", "<leader>cp", '"+p') -- Paste to clipboard in a easier way
vim.keymap.set({ "n", "v" }, "<leader>cy", '"+y') -- Copy to clipboard in a easier way
vim.keymap.set("n", "<leader>so", "<cmd>so %<cr>")
vim.keymap.set("n", "\\n", ":cn<cr>") -- Better quick fix list navigation
vim.keymap.set("n", "\\p", ":cp<cr>") -- Better quick fix list navigation
