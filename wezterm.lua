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

wezterm.on('copy-last-command-result', function(window, pane)
  -- TODO: implement the function to copy the output between the current and last prompts 
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
    fade_in_duration_ms = 80,
    fade_out_duration_ms = 80,
  },
  colors = {
    visual_bell = "#222222",
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
  disable_default_key_bindings = true,
  leader = { key = 'q', mods = 'CTRL', timeout_milliseconds = 2000 },     
  keys = {
    -- basic config
    { key = 'c',          mods = 'SUPER',             action = wezterm.action.CopyTo 'Clipboard' },
    { key = 'v',          mods = 'SUPER',             action = wezterm.action.PasteFrom 'Clipboard' },
    { key = 'x',          mods = 'SHIFT|CTRL',        action = wezterm.action.ActivateCopyMode },

    { key = 't',          mods = 'SUPER',             action = wezterm.action.SpawnTab 'CurrentPaneDomain' },
    { key = 'w',          mods = 'SUPER',             action = wezterm.action.CloseCurrentTab {confirm=true} },
    { key = '1',          mods = 'SUPER',             action = wezterm.action.ActivateTab(0) },
    { key = '2',          mods = 'SUPER',             action = wezterm.action.ActivateTab(1) },
    { key = '3',          mods = 'SUPER',             action = wezterm.action.ActivateTab(2) },
    { key = '4',          mods = 'SUPER',             action = wezterm.action.ActivateTab(3) },
    { key = '5',          mods = 'SUPER',             action = wezterm.action.ActivateTab(4) },
    { key = 'LeftArrow',  mods = 'SUPER',             action = wezterm.action.ActivateTabRelative(-1) },
    { key = 'RightArrow', mods = 'SUPER',             action = wezterm.action.ActivateTabRelative(1) },

    { key = 'n',          mods = 'SUPER',             action = wezterm.action.SpawnWindow },
    { key = 'm',          mods = 'SUPER',             action = wezterm.action.Hide },
    { key = '"',          mods = 'SHIFT|CTRL',        action = wezterm.action.SplitVertical {domain='CurrentPaneDomain'} },
    { key = '%',          mods = 'SHIFT|CTRL',        action = wezterm.action.SplitHorizontal {domain='CurrentPaneDomain'} },
    { key = 'UpArrow',    mods = 'SHIFT|CTRL|SUPER',  action = wezterm.action.AdjustPaneSize {"Up", 1} },
    { key = 'DownArrow',  mods = 'SHIFT|CTRL|SUPER',  action = wezterm.action.AdjustPaneSize {"Down", 1} },
    { key = 'LeftArrow',  mods = 'SHIFT|CTRL|SUPER',  action = wezterm.action.AdjustPaneSize {"Left", 1} },
    { key = 'RightArrow', mods = 'SHIFT|CTRL|SUPER',  action = wezterm.action.AdjustPaneSize {"Right", 1} },
    { key = 'UpArrow',    mods = 'SHIFT|CTRL',        action = wezterm.action.ActivatePaneDirection "Up" },
    { key = 'DownArrow',  mods = 'SHIFT|CTRL',        action = wezterm.action.ActivatePaneDirection "Down" },
    { key = 'LeftArrow',  mods = 'SHIFT|CTRL',        action = wezterm.action.ActivatePaneDirection "Left" },
    { key = 'RightArrow', mods = 'SHIFT|CTRL',        action = wezterm.action.ActivatePaneDirection "Right" },

    { key = '-',          mods = 'SUPER|CTRL',        action = wezterm.action.DecreaseFontSize },
    { key = '=',          mods = 'SUPER|CTRL',        action = wezterm.action.IncreaseFontSize },
    { key = '0',          mods = 'SUPER|CTRL',        action = wezterm.action.ResetFontSize },

    { key = 'UpArrow',    mods = 'SUPER|CTRL',        action = wezterm.action.ScrollToPrompt(-1) },
    { key = 'DownArrow',  mods = 'SUPER|CTRL',        action = wezterm.action.ScrollToPrompt(1) },

    -- custom actions
    { key = 'm',          mods = 'LEADER|CTRL',       action = wezterm.action.EmitEvent 'random-color-scheme' },     
    { key = 'n',          mods = 'LEADER|CTRL',       action = wezterm.action.EmitEvent 'nord-color-scheme' },     
    { key = 'c',          mods = 'LEADER|CTRL',       action = wezterm.action.EmitEvent 'copy-last-command-result' },     
  }, 
}

