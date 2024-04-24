---@alias lc.lang
---| "cpp"
---| "java"
---| "python"
---| "python3"
---| "c"
---| "csharp"
---| "javascript"
---| "typescript"
---| "php"
---| "swift"
---| "kotlin"
---| "dart"
---| "golang"
---| "ruby"
---| "scala"
---| "rust"
---| "racket"
---| "erlang"
---| "elixir"
---| "bash"

---@alias lc.hook
---| "enter"
---| "question_enter"
---| "leave"

---@alias lc.size
---| string
---| number
---| { width: string | number, height: string | number }

---@alias lc.position "top" | "right" | "bottom" | "left"

---@alias lc.direction "col" | "row"

---@alias lc.inject { before?: string|string[], after?: string|string[] }

---@alias lc.storage table<"cache"|"home", string>

---@class lc.UserConfig

require("leetcode").setup({
	---@type string
	arg = "leetcode.nvim",

	---@type lc.lang
	lang = "python3",

	cn = { -- leetcode.cn
		enabled = false, ---@type boolean
		translator = true, ---@type boolean
		translate_problems = true, ---@type boolean
	},

	---@type lc.storage
	storage = {
		home = vim.fn.stdpath("data") .. "/leetcode",
		cache = vim.fn.stdpath("cache") .. "/leetcode",
	},

	---@type table<string, boolean>
	plugins = {
		non_standalone = false,
	},

	---@type boolean
	logging = true,

	injector = {}, ---@type table<lc.lang, lc.inject>

	cache = {
		update_interval = 60 * 60 * 24 * 7, ---@type integer 7 days
	},

	console = {
		open_on_runcode = true, ---@type boolean

		dir = "row", ---@type lc.direction

		size = { ---@type lc.size
			width = "90%",
			height = "75%",
		},

		result = {
			size = "60%", ---@type lc.size
		},

		testcase = {
			virt_text = true, ---@type boolean

			size = "40%", ---@type lc.size
		},
	},

	description = {
		position = "left", ---@type lc.position

		width = "40%", ---@type lc.size

		show_stats = true, ---@type boolean
	},

	hooks = {
		---@type fun()[]
		["enter"] = {},

		---@type fun(question: lc.ui.Question)[]
		["question_enter"] = {},

		---@type fun()[]
		["leave"] = {},
	},

	keys = {
		toggle = { "q", "<Esc>" }, ---@type string|string[]
		confirm = { "<CR>" }, ---@type string|string[]

		reset_testcases = "r", ---@type string
		use_testcase = "U", ---@type string
		focus_testcases = "H", ---@type string
		focus_result = "L", ---@type string
	},

	---@type boolean
	image_support = false,
})

vim.keymap.set("n", "<leader>lr", ":Leet run<cr>")
vim.keymap.set("n", "<leader>ls", ":Leet submit<cr>")
vim.keymap.set("n", "<leader>lo", ":Leet open<cr>")
vim.keymap.set("n", "<leader>ly", ":Leet yank<cr>")
vim.keymap.set("n", "<leader>li", ":Leet info<cr>")
vim.keymap.set("n", "<leader>lc", ":Leet console<cr>")
vim.keymap.set("n", "<leader>ld", ":Leet desc<cr>")
vim.keymap.set("n", "<leader>lD", ":Leet daily<cr>")
vim.keymap.set("n", "<leader>lm", ":Leet menu<cr>")
vim.keymap.set("n", "<leader>lh", ":Leet hints<cr>")

vim.keymap.set(
	"n",
	"<leader>lle",
	":Leet list difficulty=easy<cr>"
)
vim.keymap.set(
	"n",
	"<leader>llm",
	":Leet list difficulty=medium<cr>"
)
vim.keymap.set(
	"n",
	"<leader>llh",
	":Leet list difficulty=hard<cr>"
)
