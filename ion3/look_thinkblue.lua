-- This look is based on look-awesome for Ion2.
--
-- Ported to Ion3 and modified by Austin Clements <amdragon@mit.edu>

if not gr.select_engine("de") then return end

de.reset()

mainfont = "-*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*"
black = "#000"
verydark = "#336"
dark = "#669"
light = "#99C"
white = "#FFF"

de.defstyle("*", {
    shadow_colour = verydark,
    highlight_colour = light,
    background_colour = dark,
    foreground_colour = white,
    padding_colour = light,

    -- transparent_background = false,
    transparent_background = true,
    
    border_style = "elevated",

    highlight_pixels = 0,
    shadow_pixels = 0,
    padding_pixels = 0,
    spacing = 0,
    
    font = mainfont,
    text_align = "center",
})

de.defstyle("frame", {
    based_on = "*",

    background_colour = black,

    transparent_background = true,
})

de.defstyle("frame-floating", {
    based_on = "frame",
    padding_pixels = 1,
    de.substyle("active", {
        padding_colour = light,
    }),
    de.substyle("inactive", {
        padding_colour = dark,
    }),
})

de.defstyle("tab", {
    based_on = "*",

    highlight_pixels = 1,
    shadow_pixels = 0,
    spacing = 1,

    transparent_background = true,

    text_align = "center",

    de.substyle("active-unselected", {
        shadow_colour = verydark,
        highlight_colour = dark,
        background_colour = verydark,
        foreground_colour = light,
    }),
    de.substyle("inactive-selected", {
        shadow_colour = "#666",
        highlight_colour = "#666",
        background_colour = "#333",
        foreground_colour = "#888",
    }),
    de.substyle("inactive-unselected", {
        shadow_colour = "#333",
        highlight_colour = "#333",
        background_colour = "#111",
        foreground_colour = "#777",
    }),
})

de.defstyle("tab-frame", {
    based_on = "tab",

    de.substyle("active-*-*-*-activity", {
        shadow_colour = "red",
        highlight_colour = "red",
    	background_colour = "#800",
    }),
    de.substyle("inactive-*-*-*-activity", {
        shadow_colour = "#800",
        highlight_colour = "#800",
    	background_colour = "#400",
        foreground_colour = "#888",
    }),
})

de.defstyle("tab-frame-floating", {
    based_on = "tab-frame",
    shadow_pixels = 1,
})

de.defstyle("tab-menuentry", {
    based_on = "tab",

    spacing = 1,

    font = mainfont,
    text_align = "left",
})

de.defstyle("input", {
    based_on = "*",
    
    background_colour = "#333",
})

de.defstyle("input-edln", {
    based_on = "input",

    de.substyle("*-cursor", {
        background_colour = "#FFF",
        foreground_colour = "#667",
    }),
    de.substyle("*-selection", {
        background_colour = "#AAA",
        foreground_colour = "#334",
    }),
})

de.defstyle("stdisp", {
    based_on = "input",

    -- XXX Why doesn't this work?
    --text_align = "center",

    de.substyle("important", {
        foreground_colour = "#0F0",
    }),
    de.substyle("critical", {
        foreground_colour = "#F33",
    }),
})

gr.refresh()

