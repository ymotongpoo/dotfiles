local wezterm = require 'wezterm'

-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = utf8.char(0xe0b0)

function font_with_fallback(name, params)
  local names = { name, 'Noto Sans JP', 'BIZ UDPGothic' }
  return wezterm.font_with_fallback(names, params)
end

return {
  ----------------- input
  use_ime = true,
  ime_preedit_rendering = 'System',

  ----------------- fonts
  font = font_with_fallback 'Hack Nerd Font Mono',
  font_ruled = {
    {
      italic = true,
      font = font_with_fallback 'Hack Italic',
    },
    {
      intensity = 'Bold',
      font = font_with_fallback 'Noto Sans JP Bold',
    },
  },
  font_size = 20.0,

  ----------------- window
  initial_cols = 90,
  initial_rows = 50,
  -- color_scheme = "nord",
  -- color_scheme = "Mariana",
  color_scheme = "MaterialDesignColors",
  visual_bell = {
    fade_in_function = "EaseInOut",
    fade_in_duration_ms = 150,
    fade_out_duration_ms = 100,
  },
  colors = {
    visual_bell = "#202020",
  },
  scrollback_lines = 20000,
  enable_scroll_bar = true,
  front_end = 'Software',

  ----------------- tab
  -- https://wezfurlong.org/wezterm/config/appearance.html#native-fancy-tab-bar-appearance
  hide_tab_bar_if_only_one = true,
  tab_bar_at_bottom = true,
  window_frame = {
    font = wezterm.font { family = 'Nerd', weight = 'Bold' },
    font_size = 16.0,
    active_titlebar_bg = '#aaaaaa',
    inactive_titlebar_bg = '#111111',
  },

  ----------------- check update
  check_for_update = true,
  check_for_update_interval_seconds = 86400,
  show_update_window = true,
}

