-- Based on http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=462690
-- Ben Hutchings <ben@decadent.org.uk>


-- Flash player opens full-screen windows from Firefox with size
-- request 200x200.  It closes them if they lose focus, but also if
-- they get focus too quickly.  Therefore set focus 200 ms after
-- such a window is mapped (this may need to be adjusted on slower
-- computers).

_NET_WM_STATE = ioncore.x_intern_atom('_NET_WM_STATE', false)
_NET_WM_STATE_FULLSCREEN = ioncore.x_intern_atom('_NET_WM_STATE_FULLSCREEN',
						 false)

function is_fullscreen(cwin)
   local state = ioncore.x_get_window_property(cwin:xid(), _NET_WM_STATE, 4,
					       1, true)
   if state then
      for k, v in pairs(state) do
	 if v == _NET_WM_STATE_FULLSCREEN then
	    return true
	 end
      end
   end
   return false
end

defwinprop {
   --class = "Firefox-bin",
   --class = "Iceweasel",
   class = "Npviewer.bin",      -- Firefox's 32-bit plugin adapter
   match = function(prop, cwin, id)
	      local geom = cwin:geom()
	      return is_fullscreen(cwin) and geom.w == 200 and geom.h == 200
	   end,
   switchto = false,
   flash_fullscreen = true,
}

ioncore.get_hook('clientwin_do_manage_alt'):add(
   function(cwin, table)
      --warn("Client window class: " .. cwin:get_ident()["class"])
      local winprop = ioncore.getwinprop(cwin)
      if winprop and winprop.flash_fullscreen then
	 local timer = ioncore.create_timer()
	 timer:set(200, function() cwin:goto() end)
	 return true
      else
	 return false
      end
   end
)
