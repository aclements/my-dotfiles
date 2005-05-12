MOD1="Mod3+"			-- Use the flag key
MOD2=""

ioncore.set{
   dblclick_delay=250,
   kbresize_delay=1500,
   default_ws_type="WIonWS",

   -- Enable opaque resizing so X doesn't block during resize
   opaque_resize=true,

   -- Pointer warping is nice, but the mouse can lose its position
   -- when moving between workspace
   warp=false,
}

-- Load modules
dopath("mod_query")
dopath("mod_menu")
dopath("mod_ionws")
dopath("mod_floatws")
dopath("mod_panews")
dopath("mod_statusbar")
--dopath("mod_dock")
dopath("mod_sp")

-- Load some kludges to make apps behave better
dopath("cfg_kludges")

-- Make some bindings
dopath("cfg_bindings")

-- Define some menus (mod_menu required)
dopath("cfg_menus")
