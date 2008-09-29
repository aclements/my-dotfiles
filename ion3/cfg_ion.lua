-- Ion3 master configuration
-- By Austin Clements <amdragon@mit.edu>
--
-- Written for ion-3rc-20070927

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

-- cfg_ioncore contains configuration of the Ion 'core'
dopath("cfg_ioncore")

-- Load some kludges to make apps behave better.
dopath("cfg_kludges")

-- Load modules
dopath("cfg_modules")

dopath("flashhack")
