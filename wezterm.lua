local wezterm = require 'wezterm'
local current_color_theme_index = 1
local fav_color_schemes = { 
  "Afterglow",
  "arcoiris",
  "astromouse (terminal.sexy)",
  "Atom",
  "Ayu Mirage",
  "Blazer",
  "Builtin Pastel Dark",
  "carbonfox",
  "Catppuccin Mocha",
  "ChallengerDeep",
  "Chalkboard",
  "Chester",
  "Circus (base16)",
  "Classic Dark (base16)",
  "DanQing (base16)",
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
 
function font_with_fallback(preferred, params)
  local names = preferred
  local fallbacks = { 'Hack Nerd Font Mono', 'Noto Sans JP', 'BIZ UDPGothic' }
  if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
    table.insert(fallbacks, 'Consolas')
    table.insert(fallbacks, 'Meiryo UI')
    table.insert(fallbacks, 'Yu Gothic UI')
  end
  if wezterm.target_triple == 'x86_64-apple-darwin' then
    table.insert(fallbacks, 'Monaco')
    table.insert(fallbacks, 'Menlo')
    table.insert(fallbacks, 'ヒラギノ丸ゴ ProN')
  end
  if wezterm.target_triple == 'x86_64-unknown-linux-gnu' then
    table.insert(fallbacks, 'Ubuntu Monospace')
    table.insert(fallbacks, 'DejaVu Sans')
    table.insert(fallbacks, 'Doroid Sans')
  end
  table.insert(fallbacks, 'monospace')
  for i = 1, #fallbacks do
    table.insert(names, fallbacks[i])
  end
  return wezterm.font_with_fallback(names, params)
end

function rotate_color_theme()
  current_color_theme_index = (current_color_theme_index + 1) % #fav_color_schemes
  if current_color_theme_index == 0 then current_color_theme_index = 1 end
  return fav_color_schemes[current_color_theme_index]
end

function rev_rotate_color_theme()
  current_color_theme_index = current_color_theme_index - 1
  if current_color_theme_index == 0 then current_color_theme_index = #fav_color_schemes end
  return fav_color_schemes[current_color_theme_index]
end

wezterm.on('rev-rotate-color-scheme', function(window, pane)
  local overrides = window:get_config_overrides() or {}
  scheme = rev_rotate_color_theme()
  overrides.color_scheme = scheme
  window:set_config_overrides(overrides)
  window:set_right_status(wezterm.format {
    { Text = scheme },
  })
end)

wezterm.on('rotate-color-scheme', function(window, pane)
  local overrides = window:get_config_overrides() or {}
  scheme = rotate_color_theme()
  overrides.color_scheme = scheme
  window:set_config_overrides(overrides)
  window:set_right_status(wezterm.format {
    { Text = scheme },
  })
end)

wezterm.on('nord-color-scheme', function(window, pane)
  local overrides = window:get_config_overrides() or {}
  overrides.color_scheme = 'nord'
  window:set_config_overrides(overrides)
  window:set_right_status(wezterm.format {
    { Text = scheme },
  })
end)

return {
  ----------------- input
  use_ime = true,
  ime_preedit_rendering = 'System',

  ----------------- fonts
  font = font_with_fallback { 'Cica' },
  font_size = 24.0,

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
  hide_tab_bar_if_only_one_tab = true,
  tab_bar_at_bottom = true,
  window_frame = {
    font = wezterm.font { family = 'Nerd', weight = 'Bold' },
    font_size = 16.0,
    active_titlebar_bg = '#aaaaaa',
    inactive_titlebar_bg = '#111111',
  },

  ----------------- check update
  check_for_updates = true,
  check_for_updates_interval_seconds = 86400,
  show_update_window = true,

  ----------------- mouse bindings
  mouse_bindings = {
    {
      event = { Down = { streak = 3, button = 'Left' } },
      mods = 'NONE',
      action = wezterm.action.SelectTextAtMouseCursor 'Line',
    },
    {
      event = { Down = { streak = 3, button = 'Left' } },
      action = wezterm.action.SelectTextAtMouseCursor 'SemanticZone',
      mods = 'CTRL',
    },
  },

  ----------------- key bindings
  disable_default_key_bindings = true,
  leader = { key = 'q', mods = 'CTRL', timeout_milliseconds = 2000 },     
  keys = {
    -- basic config
    { key = 'c',          mods = 'SUPER',             action = wezterm.action.CopyTo 'Clipboard' },
    { key = 'v',          mods = 'SUPER',             action = wezterm.action.PasteFrom 'Clipboard' },
    { key = 'n',          mods = 'SHIFT|CTRL',        action = wezterm.action.SpawnWindow },

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
    { key = 'UpArrow',    mods = 'SHIFT|CTRL',        action = wezterm.action.ActivatePaneDirection "Up" },
    { key = 'DownArrow',  mods = 'SHIFT|CTRL',        action = wezterm.action.ActivatePaneDirection "Down" },
    { key = 'LeftArrow',  mods = 'SHIFT|CTRL',        action = wezterm.action.ActivatePaneDirection "Left" },
    { key = 'RightArrow', mods = 'SHIFT|CTRL',        action = wezterm.action.ActivatePaneDirection "Right" },

    { key = '-',          mods = 'SUPER|CTRL',        action = wezterm.action.DecreaseFontSize },
    { key = '=',          mods = 'SUPER|CTRL',        action = wezterm.action.IncreaseFontSize },
    { key = '0',          mods = 'SUPER|CTRL',        action = wezterm.action.ResetFontSize },

    { key = 'UpArrow',    mods = 'SUPER|CTRL',        action = wezterm.action.ScrollToPrompt(-1) },
    { key = 'DownArrow',  mods = 'SUPER|CTRL',        action = wezterm.action.ScrollToPrompt(1) },

    -- change key table
    { key = 'x',          mods = 'SHIFT|CTRL',        action = wezterm.action.ActivateCopyMode },
    { key = 'p',          mods = 'SHIFT|CTRL',        action = wezterm.action.ActivateKeyTable { name = 'resize_pane', one_shot = false } },

    -- custom actions
    { key = 'p',          mods = 'LEADER|CTRL',       action = wezterm.action.EmitEvent 'rev-rotate-color-scheme' },     
    { key = 'n',          mods = 'LEADER|CTRL',       action = wezterm.action.EmitEvent 'rotate-color-scheme' },     
    { key = '0',          mods = 'LEADER|CTRL',       action = wezterm.action.EmitEvent 'nord-color-scheme' },     
    { key = 'c',          mods = 'LEADER|CTRL',       action = wezterm.action.EmitEvent 'copy-last-command-result' },     
  },

  key_tables = {
    resize_pane = {
      { key = 'h',                                    action = wezterm.action.AdjustPaneSize {"Left", 1} },
      { key = 'j',                                    action = wezterm.action.AdjustPaneSize {"Down", 1} },
      { key = 'k',                                    action = wezterm.action.AdjustPaneSize {"Up", 1} },
      { key = 'l',                                    action = wezterm.action.AdjustPaneSize {"Right", 1} },
      { key = 'LeftArrow',                            action = wezterm.action.AdjustPaneSize {"Left", 1} },
      { key = 'DownArrow',                            action = wezterm.action.AdjustPaneSize {"Down", 1} },
      { key = 'UpArrow',                              action = wezterm.action.AdjustPaneSize {"Up", 1} },
      { key = 'RightArrow',                           action = wezterm.action.AdjustPaneSize {"Right", 1} },
      { key = 'Escape',                               action = 'PopKeyTable' },
    },

    copy_mode = {
      { key = 'Space',    mods = 'NONE',              action = wezterm.action.CopyMode { SetSelectionMode = 'Cell' }, },
      { key = 'h',                                    action = wezterm.action.CopyMode 'MoveLeft' },
      { key = 'j',                                    action = wezterm.action.CopyMode 'MoveDown' },
      { key = 'k',                                    action = wezterm.action.CopyMode 'MoveUp' },
      { key = 'l',                                    action = wezterm.action.CopyMode 'MoveRight' },
      { key = 'LeftArrow',                            action = wezterm.action.CopyMode 'MoveLeft' },
      { key = 'DownArrow',                            action = wezterm.action.CopyMode 'MoveDown' },
      { key = 'UpArrow',                              action = wezterm.action.CopyMode 'MoveUp' },
      { key = 'RightArrow',                           action = wezterm.action.CopyMode 'MoveRight' },
      
      { key = 'j',        mods = 'CTRL',              action = wezterm.action.CopyMode 'MoveForwardSemanticZone' },
      { key = 'k',        mods = 'CTRL',              action = wezterm.action.CopyMode 'MoveBackwardSemanticZone' },
      { key = 'j',        mods = 'ALT',               action = wezterm.action.CopyMode { MoveBackwardZoneOfType ='Output' }},
      { key = 'k',        mods = 'ALT',               action = wezterm.action.CopyMode { MoveForwardZoneOfType ='Output' }},
      { key = 'z',        mods = 'CTRL',              action = wezterm.action.CopyMode { SetSelectionMode = 'SemanticZone' } },

      { key = 'Escape',   mods = 'NONE',              action = wezterm.action.CopyMode 'Close' },
      { key = 'c',        mods = 'CTRL',              action = wezterm.action.CopyMode 'Close' },
    },
  },
}

