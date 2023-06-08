---@diagnostic disable: deprecated
local function attach_to_buffer(output_bufnr, pattern, command)
	vim.api.nvim_create_autocmd("BufWritePost", {
		group = vim.api.nvim_create_augroup("AutoRun", { clear = true }),
		pattern = pattern,
		callback = function()
			local data_to_append = function(_, data)
				if data then
					vim.api.nvim_buf_set_lines(output_bufnr, -1, -1, false, data)
				end
			end
			vim.api.nvim_buf_set_lines(output_bufnr, 0, -1, false, { "AutoRun Start", "" })
			vim.fn.jobstart(command, {
				stdout_buffered = true,
				on_stderr = data_to_append,
				on_stdout = data_to_append,
			})
		end,
	})
end

vim.api.nvim_create_user_command("AutoRun", function(input)
	vim.cmd(input.fargs[1])
	local win = vim.api.nvim_get_current_win()
	local buf = vim.api.nvim_create_buf(true, true)
	vim.api.nvim_win_set_buf(win, buf)

	local pattern = input.fargs[2]
	local command = { unpack(input.fargs, 3, #input.fargs) }
	attach_to_buffer(buf, pattern, command)
end, { nargs = "+" })
