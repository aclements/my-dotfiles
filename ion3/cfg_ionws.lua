-- The frame control bindings are loosely based on Emacs

-- Perform focus changes in a manner similar to WIonWS.goto_dir, but
-- left/right motion can go across Xinerama screen boundaries.  Also,
-- because this tends to lead to wide worlds, left/right motion wraps
-- (though up/down motion doesn't).

function WIonWS.goto_dir_xinerama(ws, dir)
   if dir == "left" or dir == "right" then
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
      ws:goto_dir_nowrap(dir)
   end
end

defbindings("WIonWS", {
	       bdoc("Close the current frame"),
	       kpress(MOD1.."0", "WIonWS.unsplit_at(_, _sub)"),

	       bdoc("Split window vertically"),
	       kpress(MOD1.."2",
		      "WIonWS.split_at(_, _sub, 'bottom', true)"),

	       bdoc("Split window horizontally"),
	       kpress(MOD1.."3",
		      "WIonWS.split_at(_, _sub, 'right', true)"),

	       bdoc("Go to frame above/below/right/left with cursor keys"),
	       kpress(MOD1.."Up",
		      "WIonWS.goto_dir_xinerama(_, 'above')"),
	       kpress(MOD1.."Down",
		      "WIonWS.goto_dir_xinerama(_, 'below')"),
	       kpress(MOD1.."Right",
		      "WIonWS.goto_dir_xinerama(_, 'right')"),
	       kpress(MOD1.."Left",
		      "WIonWS.goto_dir_xinerama(_, 'left')"),

	       bdoc("Go to frame above/below/right/left with alphabetic keys"),
	       kpress(MOD1.."h",
		      "WIonWS.goto_dir_xinerama(_, 'left')"),
	       kpress(MOD1.."j",
		      "WIonWS.goto_dir_xinerama(_, 'below')"),
	       kpress(MOD1.."k",
		      "WIonWS.goto_dir_xinerama(_, 'above')"),
	       kpress(MOD1.."l",
		      "WIonWS.goto_dir_xinerama(_, 'right')"),
	    })
