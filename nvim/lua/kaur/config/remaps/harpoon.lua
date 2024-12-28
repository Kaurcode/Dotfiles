local harpoon = require("harpoon")

-- REQUIRED
harpoon:setup()
-- REQUIRED

vim.keymap.set("n", "<leader>yu", function() harpoon:list():replace_at(1) end)
vim.keymap.set("n", "<leader>yi", function() harpoon:list():replace_at(2) end)
vim.keymap.set("n", "<leader>yo", function() harpoon:list():replace_at(3) end)
vim.keymap.set("n", "<leader>yp", function() harpoon:list():replace_at(4) end)
vim.keymap.set("n", "<leader>yy", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end)

vim.keymap.set("n", "<leader>u", function() harpoon:list():select(1) end)
vim.keymap.set("n", "<leader>i", function() harpoon:list():select(2) end)
vim.keymap.set("n", "<leader>o", function() harpoon:list():select(3) end)
vim.keymap.set("n", "<leader>p", function() harpoon:list():select(4) end)

-- Toggle previous & next buffers stored within Harpoon list
-- vim.keymap.set("n", "<leader>m", function() harpoon:list():prev() end)
-- vim.keymap.set("n", "<leader>,", function() harpoon:list():next() end)
