--
-- Ion tiling module configuration file
--

-- The frame control bindings are loosely based on Emacs

-- Perform focus changes in a manner similar to WIonWS.goto_dir, but
-- left/right motion can go across Xinerama screen boundaries.  Also,
-- because this tends to lead to wide worlds, left/right motion wraps
-- (though up/down motion doesn't).

-- XXX Haven't tested xinerama switching with new Ion
use_xinerama_switch = 0

function ioncore.goto_dir_xinerama(ws, dir)
   if (not use_xinerama_switch == 0) and (dir == "left" or dir == "right") then
      local nextwindow = ws:nextto(ws:current(), dir)

      if not nextwindow then
         local curscreen = ws:screen_of()
         local offset, oppositedir

         if dir == "right" then
            offset = 1
            oppositedir = "left"
         else
            offset = -1
            oppositedir = "right"
         end

         local nextscreen = ioncore.find_screen_id(curscreen:id()+offset)

         if not nextscreen then
            -- Went off right side (find_screen_id(-1) already wraps)
            nextscreen = ioncore.find_screen_id(0)
         end

         local nextws = nextscreen:current()

         if not obj_is(nextws, "WIonWS") then
            -- Just go to the screen.  This is a bit awkward because
            -- it's probably irreversible
            ioncore.goto_nth_screen(nextscreen)
            return
         else
            nextwindow = nextws:farthest(oppositedir)
         end
      end

      nextwindow:goto()
   else
      ioncore.goto_next(ws, dir, {nowrap=1})
   end
end

defbindings("WTiling", {
	       bdoc("Close the current frame"),
	       kpress(MOD1.."0", "WTiling.unsplit_at(_, _sub)"),

	       bdoc("Split window vertically"),
	       kpress(MOD1.."2",
		      "WTiling.split_at(_, _sub, 'bottom', true)"),

	       bdoc("Split window horizontally"),
	       kpress(MOD1.."3",
		      "WTiling.split_at(_, _sub, 'right', true)"),

	       bdoc("Go to frame above/below/right/left with cursor keys"),
	       kpress(MOD1.."Up",
		      "ioncore.goto_dir_xinerama(_sub, 'up')"),
	       kpress(MOD1.."Down",
		      "ioncore.goto_dir_xinerama(_sub, 'down')"),
	       kpress(MOD1.."Right",
		      "ioncore.goto_dir_xinerama(_sub, 'right')"),
	       kpress(MOD1.."Left",
		      "ioncore.goto_dir_xinerama(_sub, 'left')"),

	       bdoc("Go to frame above/below/right/left with alphabetic keys"),
	       kpress(MOD1.."h",
		      "ioncore.goto_dir_xinerama(_sub, 'left')"),
	       kpress(MOD1.."j",
		      "ioncore.goto_dir_xinerama(_sub, 'down')"),
	       kpress(MOD1.."k",
		      "ioncore.goto_dir_xinerama(_sub, 'up')"),
	       kpress(MOD1.."l",
		      "ioncore.goto_dir_xinerama(_sub, 'right')"),
	    })

-- Context menu for tiled workspaces.

defctxmenu("WTiling", "Tiling", {
    menuentry("Destroy frame", 
              "WTiling.unsplit_at(_, _sub)"),

    menuentry("Split vertically", 
              "WTiling.split_at(_, _sub, 'bottom', true)"),
    menuentry("Split horizontally", 
              "WTiling.split_at(_, _sub, 'right', true)"),
    
    menuentry("Flip", "WTiling.flip_at(_, _sub)"),
    menuentry("Transpose", "WTiling.transpose_at(_, _sub)"),
    
    submenu("Float split", {
        menuentry("At left", 
                  "WTiling.set_floating_at(_, _sub, 'toggle', 'left')"),
        menuentry("At right", 
                  "WTiling.set_floating_at(_, _sub, 'toggle', 'right')"),
        menuentry("Above",
                  "WTiling.set_floating_at(_, _sub, 'toggle', 'up')"),
        menuentry("Below",
                  "WTiling.set_floating_at(_, _sub, 'toggle', 'down')"),
    }),

    submenu("At root", {
        menuentry("Split vertically", 
                  "WTiling.split_top(_, 'bottom')"),
        menuentry("Split horizontally", 
                  "WTiling.split_top(_, 'right')"),
        menuentry("Flip", "WTiling.flip_at(_)"),
        menuentry("Transpose", "WTiling.transpose_at(_)"),
    }),
})

-- Context menu entries for tiled frames.

defctxmenu("WFrame.tiled", "Tiled frame", {
    menuentry("Detach", "mod_tiling.detach(_sub)", "_sub:non-nil"),
})


-- Extra context menu extra entries for floatframes. 

defctxmenu("WFrame.floating", "Floating frame", {
    append=true,
    menuentry("New tiling", "mod_tiling.mkbottom(_)"),
})

-- Adjust default workspace layout

local a_frame = {
    type="WSplitRegion",
    regparams = {
        type = "WFrame", 
        frame_style = "frame-tiled"
    }
}

local screen_width = ioncore.find_screen_id(0):geom()["w"]
local emacs_width = 500

ioncore.set{
    default_ws_params = {
        -- Destroy workspace if the 'bottom' tiling is destroyed last
        bottom_last_close = true,
        -- Layout
        managed = {
            {
                type = "WTiling",
                bottom = true,
                -- The default is a single horizontal split, where the
                -- left is wide enough for an 80 column emacs
                split_tree = {
                    type = "WSplitSplit",
                    dir = "horizontal",
                    tls = emacs_width,
                    brs = screen_width - emacs_width,
                    tl = a_frame,
                    br = a_frame
                }
                -- For a single frame
                --split_tree = nil
            }
        }
    }
}
