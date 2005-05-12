-- look-awesome.lua drawing engine configuration file for Ion.

if not gr_select_engine("de") then return end

de_reset()

de_define_style("*", {
    shadow_colour = "#99A",
    highlight_colour = "#99A",
    background_colour = "#667",
    foreground_colour = "#FFF",
    padding_colour = "#99A",

    transparent_background = false,
    
    border_style = "elevated",

    highlight_pixels = 0,
    shadow_pixels = 0,
    padding_pixels = 0,
    spacing = 0,
    
    font = "-xos4-terminus-medium-r-normal--14-*-*-*-*-*-*-*",
    text_align = "center",
})

de_define_style("frame", {
    based_on = "*",

-- this sets the color between tabs as well, I could not figure out any way to make it transparent
    background_colour = "black",

    transparent_background = true,

--    de_substyle("active", {
--    }),
--    de_substyle("inactive", {
--    }),
})

de_define_style("frame-ionframe", {
    based_on = "frame",
--    de_substyle("active", {
--    }),
--    de_substyle("inactive", {
--    }),
})

de_define_style("frame-floatframe", {
    based_on = "frame",
    padding_pixels = 1,
    de_substyle("active", {
        padding_colour = "#99A",
    }),
    de_substyle("inactive", {
        padding_colour = "#666",
    }),
})

de_define_style("tab", {
    based_on = "*",

    highlight_pixels = 1,
    shadow_pixels = 1,
    padding_pixels = 1,
    spacing = 1,

    transparent_background = true,

    text_align = "center",

    de_substyle("active-selected", {
        shadow_colour = "#99A",
        highlight_colour = "#99A",
        background_colour = "#667",
        foreground_colour = "#FFF",
    }),
    de_substyle("active-unselected", {
        shadow_colour = "#667",
        highlight_colour = "#667",
        background_colour = "#334",
        foreground_colour = "#999",
    }),
    de_substyle("inactive-selected", {
        shadow_colour = "#666",
        highlight_colour = "#666",
        background_colour = "#333",
        foreground_colour = "#888",
    }),
    de_substyle("inactive-unselected", {
        shadow_colour = "#333",
        highlight_colour = "#333",
        background_colour = "#111",
        foreground_colour = "#777",
    }),
})

de_define_style("tab-frame", {
    based_on = "tab",
    padding_pixels = 3,
--    de_substyle("*-*-tagged", {
--    }),
--    de_substyle("*-*-*-dragged", {
--    }),
    de_substyle("active-*-*-*-activity", {
        shadow_colour = "red",
        highlight_colour = "red",
    	background_colour = "#800",
        foreground_colour = "#FFF",
    }),
    de_substyle("inactive-*-*-*-activity", {
        shadow_colour = "#800",
        highlight_colour = "#800",
    	background_colour = "#400",
        foreground_colour = "#888",
    }),
})

de_define_style("tab-frame-ionframe", {
    based_on = "tab-frame",
})

de_define_style("tab-frame-floatframe", {
    based_on = "tab-frame",
    padding_pixels = 4,
})

de_define_style("tab-menuentry", {
    based_on = "tab",

    padding_pixels = 6,
    spacing = 4,

    font = "-xos4-terminus-medium-r-normal--16-*-*-*-*-*-*-*",
    text_align = "left",

--    de_substyle("*-*-submenu", {
--    }),
})

de_define_style("tab-menuentry-big", {
    based_on = "tab-menuentry",

    padding_pixels = 8,

    font = "-xos4-terminus-medium-r-normal--28-*-*-*-*-*-*-*",
})

de_define_style("input", {
    based_on = "*",
    
    foreground_colour = "#FFF",
    background_colour = "#667",
    padding_colour = "#667",

    transparent_background = false,

    border_style = "elevated",

    padding_pixels = 2,
})

de_define_style("input-edln", {
    based_on = "input",

    de_substyle("*-cursor", {
        background_colour = "#FFF",
        foreground_colour = "#667",
    }),
    de_substyle("*-selection", {
        background_colour = "#AAA",
        foreground_colour = "#334",
   }),
})

de_define_style("input-message", {
    based_on = "input",
})

de_define_style("input-menu", {
    based_on = "input",

    transparent_background = true,

    highlight_pixels = 0,
    shadow_pixels = 0,
    padding_pixels = 0,
    spacing = 0,
})

de_define_style("input-menu-big", {
    based_on = "input-menu",
})

de_define_style("moveres_display", {
    based_on = "input",
})

de_define_style("dock", {
    based_on = "*",
})

gr_refresh()

