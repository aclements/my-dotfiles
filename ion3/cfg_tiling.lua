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

-- Based on http://modeemi.fi/~tuomov/repos/ion-scripts-3/scripts/move_current.lua
function move_current(region, dir)
   local cur = region:current()
   local mgr = region:manager()

   if not cur or not obj_is(mgr, "WTiling") then 
      return
   end

   local target = mgr:nextto(region, dir)
   if target then
      target:attach(cur, { switchto=true })
      target:goto()
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

	       bdoc("Move frame up/down/right/left with alphabetic keys"),
	       kpress(MOD1.."Shift+h",
		      "move_current(_sub, 'left')"),
	       kpress(MOD1.."Shift+j",
		      "move_current(_sub, 'down')"),
	       kpress(MOD1.."Shift+k",
		      "move_current(_sub, 'up')"),
	       kpress(MOD1.."Shift+l",
		      "move_current(_sub, 'right')"),
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
    
    menuentry("Untile", "mod_tiling.untile(_)"),
    
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

-- Add workspace layouts

local screen_width = ioncore.find_screen_id(0):geom()["w"]
local screen_height = ioncore.find_screen_id(0):geom()["h"]
local emacs_width = 500
local term_width = 482
local term_height = 375

local a_frame = {
    type="WSplitRegion",
    regparams = {
        type = "WFrame", 
        frame_style = "frame-tiled"
    }
}
local emacs_layout = {
   type = "WTiling",
   bottom = true,
   -- Single horizontal split, where the right is wide enough for an
   -- 80 column terminal
   split_tree = {
      type = "WSplitSplit",
      dir = "horizontal",
      tls = screen_width - term_width,
      brs = term_width,
      tl = a_frame,
      -- Single vertical split, where the bottom is tall enough for a
      -- 36 row terminal
      br = {
         type = "WSplitSplit",
         dir = "vertical",
         tls = screen_height - term_height,
         brs = term_height,
         tl = a_frame,
         br = a_frame
      }
   }
}

ioncore.deflayout("emacs", { managed = { emacs_layout } })
