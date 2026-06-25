-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices.

-- For example, changing the initial geometry for new windows:
config.initial_cols = 120
config.initial_rows = 28

-- or, changing the font size and color scheme.
config.font_size = 14
-- config.color_scheme = 'AdventureTime'
config.color_scheme = 'City Lights (Gogh)'
-- config.color_scheme = 'synthwave-everything'
-- config.color_scheme = 'Synthwave (Gogh)'
-- config.color_scheme = 'SynthwaveAlpha'
config.font = wezterm.font 'FiraCode Nerd Font Mono'
config.window_background_opacity = 0.9

config.window_frame = {
  -- The font used in the tab bar.
  -- Roboto Bold is the default; this font is bundled
  -- with wezterm.
  -- Whatever font is selected here, it will have the
  -- main font setting appended to it to pick up any
  -- fallback fonts you may have used there.
  font = wezterm.font { family = 'Roboto', weight = 'Bold' },

  -- The size of the font in the tab bar.
  -- Default to 10.0 on Windows but 12.0 on other systems
  font_size = 12.0,

  -- The overall background color of the tab bar when
  -- the window is focused
  active_titlebar_bg = '#333333',

  -- The overall background color of the tab bar when
  -- the window is not focused
  inactive_titlebar_bg = '#333333',
}

config.key_map_preference = "Physical"

config.enable_wayland = true
config.front_end = "WebGpu"
config.hide_tab_bar_if_only_one_tab = true
config.enable_scroll_bar = true
config.scrollback_lines = 10000
config.adjust_window_size_when_changing_font_size = false

config.window_padding = {
  left = "5cell",
  right = "5cell",
  top = "2.5cell",
  bottom = "2.5cell",
}

config.colors = {
  background = '#1a1a1a',
}

-- Finally, return the configuration to wezterm:
return config
