local wezterm = require 'wezterm'

function font_with_fallback(name, params)
  local names = { name, 'Noto Sans JP', 'BIZ UDPGothic', 'monospace' }
  return wezterm.font_with_fallback(names, params)
end

function random_color_scheme()
  math.randomseed(os.time())
  local schemes = { 
    "Afterglow",
    "arcoiris",
    "Arthur",
    "Atom",
    "Blazer",
    "Builtin Pastel Dark",
    "Chalkboard",
    "Chester",
    "DoomOne",
    "Dracula+",
    "FishTank",
    "Guezwhoz",
    "iceberg-dark",
    "Japanesque",
    "kanagawabones",
    "lovelace",
    "Mariana",
    "MaterialDesignColors",
    "neobones_dark",
    "nord",
    "OneHalfDark",
    "Overnight Slumber",
    "Pnevma",
    "Rapture",
    "rebecca",
    "Smyck",
    "Teerb",
    "tokyonight-storm",
    "Tomorrow Night Eighties",
    "Whimsy",
    "Wombat",
  }
  local i = math.random(#schemes) 
  return schemes[i]
end

wezterm.on('random-color-scheme', function(window, pane)
  local overrides = window:get_config_overrides() or {}
  scheme = random_color_scheme()
  overrides.color_scheme = scheme
  window:set_config_overrides(overrides)
end)

wezterm.on('nord-color-scheme', function(window, pane)
  local overrides = window:get_config_overrides() or {}
  overrides.color_scheme = 'nord'
  window:set_config_overrides(overrides)
end)

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
  color_scheme = "nord",

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

  ----------------- key bindings
  leader = { key = 'q', mods = 'CTRL', timeout_milliseconds = 2000 },     
  keys = {
    { key = 'c',          mods = 'LEADER|CTRL',       action = wezterm.action.EmitEvent 'random-color-scheme' },     
    { key = 'n',          mods = 'LEADER|CTRL',       action = wezterm.action.EmitEvent 'nord-color-scheme' },     
    { key = 'UpArrow',    mods = 'SHIFT|CTRL',        action = wezterm.action.ActivateTabRelative(-1) },
    { key = 'DownArrow',  mods = 'SHIFT|CTRL',        action = wezterm.action.ActivateTabRelative(1) },
  }, 
}

