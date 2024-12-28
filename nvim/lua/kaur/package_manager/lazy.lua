require("kaur.package_manager.lazy_install")

require("lazy").setup({
  spec = {
    -- import your plugins
    { import = "kaur.package_manager.plugins_install" },
  },
  defaults = {
    lazy = false,
    version = false,
  },
  -- Configure any other settings here. See the documentation for more details.
  -- colorscheme that will be used when installing plugins.
  install = { colorscheme = { "habamax" } },
  -- automatically check for plugin updates
  checker = { enabled = true },
})
