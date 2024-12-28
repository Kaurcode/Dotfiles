vim.g.mapleader = " "

local path = "kaur.config."

require(path .. "options")
require("kaur.package_manager.lazy")
require(path .. "plugin_configs")
require(path .. "remaps.main")
require(path .. "theme")

