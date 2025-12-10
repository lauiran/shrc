-- Pull in the wezterm API
local wezterm = require 'wezterm'
local act=wezterm.action

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
--config.color_scheme_dirs = { '~/.config/wezterm/colors' }
-- config.color_scheme = 'Google (light) (terminal.sexy)'
--config.color_scheme = 'e6e1'

config.font =
wezterm.font("DejaVu Sans Mono for Powerline", {weight="Regular", stretch="Normal", style="Normal"})

config.font_size = 10.5
config.cursor_blink_rate = 1000
config.default_cursor_style = "BlinkingBlock"
config.window_background_opacity = 0.94

config.initial_cols = 192
config.initial_rows = 54

config.show_tab_index_in_tab_bar = false
config.use_fancy_tab_bar = true
config.show_new_tab_button_in_tab_bar = true

-- key bind
config.keys = {
    {
        key = "F2",
        mods = "NONE",
        action = act.PromptInputLine {
            description = "Enter new tab name:",
            action = wezterm.action_callback(function(window, pane, line)
                if line then
                    window:active_tab():set_title(line)
                end
            end),
        },
    },
}

--config.domains = {
--    {
--        name = '25.smnq',
--        connect_automatically = false,
--        local_pane = {
--            command = 'telnet',
--            args = { '10.227.168.40', '10025' },
--        },
--    },
--}

config.colors = {
    foreground = "black",
    background = "#e6e6e1",

    cursor_bg = "black",
    cursor_fg = "#e6e6e1",
    cursor_border = "#28282d",

    selection_bg = "#c8c8c3",       -- s:bg3 (light3)
    selection_fg = "#28282d",

    scrollbar_thumb = "#909095",    -- s:gray

    --ansi = {
    --    "#000000",  -- color0 (black)
    --    "#870000",  -- color1 (faded_red)
    --    "#228635",  -- color2 (faded_green)
    --    "#276678",  -- color3 (faded_yellow)
    --    "#005cc0",  -- color4 (faded_blue)
    --    "#af5f00",  -- color5 (faded_orange)
    --    "#178688",  -- color6 (faded_aqua)
    --    "#e6e6e1",  -- color7 (white)
    --},

    --brights = {
    --    "#767676",  -- color8 (bright_black)
    --    "#af0000",  -- color9 (neutral_red)
    --    "#87af00",  -- color10 (neutral_green)
    --    "#d78700",  -- color11 (neutral_yellow)
    --    "#5f8787",  -- color12 (neutral_blue)
    --    "#d75f00",  -- color13 (neutral_orange)
    --    "#5faf87",  -- color14 (neutral_aqua)
    --    "#ffffff",  -- color15 (bright_white)
    --},

    ansi = {
        "#e6e6e1",  -- 0: terminal_color_0 (bg0)
        "#af0000",  -- 1: neutral_red
        "#87af00",  -- 2: neutral_green
        "#d78700",  -- 3: neutral_yellow
        "#5f8787",  -- 4: neutral_blue
        "#af5f87",  -- 5: neutral_purple
        "#5faf87",  -- 6: neutral_aqua
        "#78787d",  -- 7: fg4 (dark4)
    },

    brights = {
        "#909095",  -- 8: gray
        "#870000",  -- 9: faded_red
        "#228635",  -- 10: faded_green
        "#276678",  -- 11: faded_yellow
        "#005cc0",  -- 12: faded_blue
        "#855080",  -- 13: faded_purple
        "#178688",  -- 14: faded_aqua
        "#3c3c41",  -- 15: fg1 (dark1)
    },

    tab_bar = {
        background = "#28282d",
        active_tab = {
            bg_color = "#e6e6e1",
            fg_color = "#28282d",
        },
        inactive_tab = {
            bg_color = "#48484d",
            fg_color = "#909095",
        },
        inactive_tab_hover = {
            bg_color = "#909095",
            fg_color = "#48484d",
        },
        new_tab = {
            bg_color = "#909095",
            fg_color = "#48484d",
        },
        new_tab_hover = {
            bg_color = "#e6e6e1",
            fg_color = "#28282d",
        },
    },
}

-- and finally, return the configuration to wezterm
return config

