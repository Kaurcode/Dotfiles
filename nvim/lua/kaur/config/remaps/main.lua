vim.keymap.set("n", "<leader>e", vim.cmd.Ex)
vim.api.nvim_set_keymap("i", "jj", "<Esc>", {noremap=false})

local path = "kaur.config.remaps."

require(path .. "telescope")
require(path .. "harpoon")
