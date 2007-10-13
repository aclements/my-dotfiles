-- Ion3 bindings
-- By Austin Clements <amdragon@mit.edu>


-- WScreen context bindings
--
-- The bindings in this context are available all the time.

defbindings("WScreen", {
	       bdoc("Switch to n:th object (workspace, full screen client window) "..
		    "within current screen."),
	       kpress(MOD1.."F1", "WScreen.switch_nth(_, 0)"),
	       kpress(MOD1.."F2", "WScreen.switch_nth(_, 1)"),
	       kpress(MOD1.."F3", "WScreen.switch_nth(_, 2)"),
	       kpress(MOD1.."F4", "WScreen.switch_nth(_, 3)"),
	       kpress(MOD1.."F5", "WScreen.switch_nth(_, 4)"),
	       kpress(MOD1.."F6", "WScreen.switch_nth(_, 5)"),
	       kpress(MOD1.."F7", "WScreen.switch_nth(_, 6)"),
	       kpress(MOD1.."F8", "WScreen.switch_nth(_, 7)"),
	       kpress(MOD1.."F9", "WScreen.switch_nth(_, 8)"),
	       kpress(MOD1.."F10", "WScreen.switch_nth(_, 9)"),
	       kpress(MOD1.."F11", "WScreen.switch_nth(_, 10)"),
	       kpress(MOD1.."F12", "WScreen.switch_nth(_, 11)"),

	       bdoc("Switch to next/previous object within current screen."),
	       kpress(MOD1.."comma", "WScreen.switch_prev(_)"),
	       kpress(MOD1.."period", "WScreen.switch_next(_)"),

	       bdoc("Toggle between two active objects."),
	       kpress(MOD1.."space", "ioncore.goto_previous()"),

	       bdoc("Query for workspace to go to or create a new one."),
	       kpress(MOD2.."F9", "mod_query.query_workspace(_)"),

	       bdoc("Display the main menu."),
	       kpress(MOD2.."F12",
                      "mod_menu.menu(_, _sub, 'mainmenu', {big=true})"),
	       mpress("Button3", "mod_menu.pmenu(_, _sub, 'mainmenu')"),

	       bdoc("Forward-circulate focus."),
	       kpress(MOD1.."Tab", "ioncore.goto_next(_chld, 'right')", 
		      "_chld:non-nil"),
})

-- Client window bindings
--
-- These bindings affect client windows directly.

defbindings("WClientWin", {
               bdoc("Quote the next key press"),
               kpress(MOD1.."Q", "WClientWin.quote_next(_)"),
               
	       bdoc("Nudge current client window. This might help with some "..
		    "programs' resizing problems."),
	       kpress_wait(MOD1.."P", "WClientWin.nudge(_)"),
})

-- Client window group bindings

function WRegion.smart_fullscreen(region)
   local ws = ioncore.lookup_region("Fullscreen", "WGroupWS")
   if not ws then
      local scr = region:screen_of()
      local fullscreen = {
         type = "WTiling",
         bottom = true,
         split_tree = {
            type = "WSplitRegion",
            regparams = {
               type = "WFrame",
               frame_style = "frame-tiled" } } }
      ws = ioncore.create_ws(scr, { managed = { fullscreen } })
      ws:set_name("Fullscreen")
   end
   local function find_and_attach (m)
      if obj_is(m, "WRegion") then
         ioncore.defer(function ()
                          WMPlex.attach(m:current(), region,
                                        {switchto=true})
                          ws:goto()
                       end)
         return false
      else
         return true
      end
   end
   if ws:managed_i(find_and_attach) then
      ioncore.warn("Fullscreen frame not found")
   end
end

defbindings("WGroupCW", {
	       bdoc("Toggle client window group full-screen mode"),
               kpress(MOD1.."1", "WRegion.smart_fullscreen(_)"),
            })

-- WMPlex context bindings
--
-- These bindings work in frames and on screens. The innermost of such
-- contexts/objects always gets to handle the key press. Most of these 
-- bindings define actions on client windows. (Remember that client windows 
-- can be put in fullscreen mode and therefore may not have a frame.)

defbindings("WMPlex", {
               bdoc("Detach (float) or reattach an object to its previous location."),
               kpress(MOD1.."F", "ioncore.detach(_chld, 'toggle')", "_chld:non-nil"),
            })

-- Frames for transient windows ignore this bindmap

defbindings("WMPlex.toplevel", {
               bdoc("Toggle tag of current object."),
               kpress(META.."T", "WRegion.set_tagged(_sub, 'toggle')", "_sub:non-nil"),

	       bdoc("Query for a client window to go to."),
	       kpress(MOD1.."G", "mod_query.query_gotoclient(_)"),

	       bdoc("Query for manual page to be displayed."),
	       kpress(MOD2.."F1", "mod_query.query_man(_, ':man')"),

	       bdoc("Run a terminal emulator."),
	       kpress(MOD2.."F2", "ioncore.exec_on(_, '$XTERMCMD')"),
    
	       bdoc("Query for command line to execute."),
	       kpress(MOD2.."F3", "mod_query.query_exec(_)"),

	       bdoc("Query for host to connect to with SSH."),
	       kpress(MOD2.."F4", "mod_query.query_ssh(_, ':ssh')"),

	       bdoc("Query for file to edit."),
	       kpress(MOD2.."F5", "mod_query.query_editfile"..
                      "(_, 'run-mailcap --action=edit')"),

	       bdoc("Query for file to view."),
	       kpress(MOD2.."F6", "mod_query.query_runfile"..
                      "(_, 'run-mailcap --action=view')"),

               bdoc("Query for a lua expression to evaluate."),
               kpress(MOD2.."F7", "mod_query.query_lua(_)"),

               bdoc("Xlock"),
               kpress(MOD2.."F8", "ioncore.exec_on(_, 'xscreensaver-command -lock')"),
	    })

-- WFrame context bindings
--
-- These bindings are common to all types of frames. The rest of frame
-- bindings that differ between frame types are defined in the modules' 
-- configuration files.

defbindings("WFrame", {
	       bdoc("Maximize the frame vertically."),
	       kpress(MOD1.."V", "WFrame.maximize_vert(_)"),

	       bdoc("Display frame context menu."),
	       mpress("Button3", "mod_menu.pmenu(_, _sub, 'ctxmenu')"),

	       bdoc("Begin move/resize mode."),
	       kpress(MOD1.."R", "WFrame.begin_kbresize(_)"),

	       bdoc("Switch the frame to display the object indicated by "..
		    "the tab."),
	       mclick("Button1@tab", "WFrame.p_switch_tab(_)"),
	       mclick("Button2@tab", "WFrame.p_switch_tab(_)"),

	       bdoc("Resize the frame."),
	       mdrag("Button1@border", "WFrame.p_resize(_)"),
	       mdrag(MOD1.."Button3", "WFrame.p_resize(_)"),

	       bdoc("Move the frame."),
	       mdrag(MOD1.."Button1", "WFrame.p_move(_)"),

	       bdoc("Move objects between frames by dragging and dropping "..
		    "the tab."),
	       mdrag("Button1@tab", "WFrame.p_tabdrag(_)"),
	       mdrag("Button2@tab", "WFrame.p_tabdrag(_)"),
	    })

-- Frames for transient windows ignore this bindmap

defbindings("WFrame.toplevel", {
	       bdoc("Switch to next/previous object within the frame."),
	       kpress(MOD1.."I", "WFrame.switch_next(_)"),
	       kpress(MOD1.."U", "WFrame.switch_prev(_)"),

               bdoc("Move current object within the frame left/right."),
               kpress(MOD1.."Shift+I", "WFrame.dec_index(_, _sub)", "_sub:non-nil"),
               kpress(MOD1.."Shift+U", "WFrame.inc_index(_, _sub)", "_sub:non-nil"),

	       bdoc("Maximize the frame vertically."),
	       kpress(MOD1.."V", "WFrame.maximize_vert(_)"),

	       bdoc("Attach tagged objects to this frame."),
	       kpress(MOD1.."A", "ioncore.tagged_attach(_)"),
})

-- Bindings for floating frames.

defbindings("WFrame.floating", {
    bdoc("Toggle shade mode"),
    mdblclick("Button1@tab", "WFrame.set_shaded(_, 'toggle')"),
    
    bdoc("Raise the frame."),
    mpress("Button1@tab", "WRegion.rqorder(_, 'front')"),
    mpress("Button1@border", "WRegion.rqorder(_, 'front')"),
    mclick(META.."Button1", "WRegion.rqorder(_, 'front')"),
    
    bdoc("Lower the frame."),
    mclick(META.."Button3", "WRegion.rqorder(_, 'back')"),
    
    bdoc("Move the frame."),
    mdrag("Button1@tab", "WFrame.p_move(_)"),
})

-- WMoveresMode context bindings
-- 
-- These bindings are available keyboard move/resize mode. The mode
-- is activated on frames with the command begin_kbresize (bound to
-- META.."R" above by default).

defbindings("WMoveresMode", {
	       bdoc("Cancel the resize mode."),
	       kpress("AnyModifier+Escape","WMoveresMode.cancel(_)"),

	       bdoc("End the resize mode."),
	       kpress("AnyModifier+Return","WMoveresMode.finish(_)"),

	       bdoc("Grow in specified direction."),
	       kpress("Left",  "WMoveresMode.resize(_, 1, 0, 0, 0)"),
	       kpress("Right", "WMoveresMode.resize(_, 0, 1, 0, 0)"),
	       kpress("Up",    "WMoveresMode.resize(_, 0, 0, 1, 0)"),
	       kpress("Down",  "WMoveresMode.resize(_, 0, 0, 0, 1)"),
	       kpress("H",     "WMoveresMode.resize(_, 1, 0, 0, 0)"),
	       kpress("L",     "WMoveresMode.resize(_, 0, 1, 0, 0)"),
	       kpress("K",     "WMoveresMode.resize(_, 0, 0, 1, 0)"),
	       kpress("J",     "WMoveresMode.resize(_, 0, 0, 0, 1)"),

	       bdoc("Shrink in specified direction."),
	       kpress("Shift+Left",  "WMoveresMode.resize(_,-1, 0, 0, 0)"),
	       kpress("Shift+Right", "WMoveresMode.resize(_, 0,-1, 0, 0)"),
	       kpress("Shift+Up",    "WMoveresMode.resize(_, 0, 0,-1, 0)"),
	       kpress("Shift+Down",  "WMoveresMode.resize(_, 0, 0, 0,-1)"),
	       kpress("Shift+H",     "WMoveresMode.resize(_,-1, 0, 0, 0)"),
	       kpress("Shift+L",     "WMoveresMode.resize(_, 0,-1, 0, 0)"),
	       kpress("Shift+K",     "WMoveresMode.resize(_, 0, 0,-1, 0)"),
	       kpress("Shift+J",     "WMoveresMode.resize(_, 0, 0, 0,-1)"),

	       bdoc("Move in specified direction."),
	       kpress(MOD1.."Left",  "WMoveresMode.move(_,-1, 0)"),
	       kpress(MOD1.."Right", "WMoveresMode.move(_, 1, 0)"),
	       kpress(MOD1.."Up",    "WMoveresMode.move(_, 0,-1)"),
	       kpress(MOD1.."Down",  "WMoveresMode.move(_, 0, 1)"),
	       kpress(MOD1.."H",     "WMoveresMode.move(_,-1, 0)"),
	       kpress(MOD1.."L",     "WMoveresMode.move(_, 1, 0)"),
	       kpress(MOD1.."K",     "WMoveresMode.move(_, 0,-1)"),
	       kpress(MOD1.."J",     "WMoveresMode.move(_, 0, 1)"),
	    })

--
-- Menu definitions
--


-- Main menu
defmenu("mainmenu", {
    submenu("Programs",         "appmenu"),
--    menuentry("Lock screen",    "ioncore.exec_on(_, 'xlock')"),
    menuentry("Help",           "mod_query.query_man(_)"),
    menuentry("About Ion",      "mod_query.show_about_ion(_)"),
    submenu("Styles",           "stylemenu"),
    submenu("Debian",           "Debian"),
    submenu("Session",          "sessionmenu"),
})

-- Auto-generated Debian menu definitions
-- Install the "menu" package to make this work
if os.execute("test -x /usr/bin/update-menus") == 0 then
    if ioncore.is_i18n() then
        dopath("debian-menu-i18n")
    else
        dopath("debian-menu")
    end
end


-- Application menu
defmenu("appmenu", {
    menuentry("Terminal",       "ioncore.exec_on(_, '$XTERMCMD')"),
    menuentry("XTerm",          "ioncore.exec_on(_, 'xterm')"),
--    menuentry("W3M",            "ioncore.exec_on(_, ':w3m -v')"),
--    menuentry("Rxvt",           "ioncore.exec_on(_, 'rxvt')"),
--    menuentry("Opera",          "ioncore.exec_on(_, 'opera')"),
--    menuentry("Links",          "ioncore.exec_on(_, ':links')"),
--    menuentry("Konqueror",      "ioncore.exec_on(_, 'konqueror')"),
--    menuentry("Dillo",          "ioncore.exec_on(_, 'dillo')"),
    menuentry("aterm",          "ioncore.exec_on(_, 'aterm')"),
    menuentry("firefox",        "ioncore.exec_on(_, 'firefox')"),
    menuentry("Run...",         "mod_query.query_exec(_)"),
})


-- Session control menu
function shutdownDWIM()
   if os.getenv("XSESSION") then
      -- Athena is funky, as usual
      ioncore.exec('kill -HUP $XSESSION')
   else
      ioncore.shutdown()
   end
end

defmenu("sessionmenu", {
    menuentry("Save",           "ioncore.snapshot()"),
    menuentry("Restart",        "ioncore.restart()"),
    menuentry("Restart TWM",    "ioncore.restart_other('twm')"),
    menuentry("Exit",           "shutdownDWIM()"),
})


-- Context menu (frame actions etc.)
defctxmenu("WFrame", "Frame", {
    -- Note: this propagates the close to any subwindows; it does not
    -- destroy the frame itself, unless empty. An entry to destroy tiled
    -- frames is configured in cfg_tiling.lua.
    menuentry("Close",          "WRegion.rqclose_propagate(_, _sub)"),
    -- Low-priority entries
    menuentry("Attach tagged", "ioncore.tagged_attach(_)", { priority = 0 }),
    menuentry("Clear tags",    "ioncore.tagged_clear()", { priority = 0 }),
    menuentry("Window info",   "mod_query.show_tree(_, _sub)", { priority = 0 })
,
})


-- Context menu for groups (workspaces, client windows)
defctxmenu("WGroup", "Group", {
    menuentry("Toggle tag",     "WRegion.set_tagged(_, 'toggle')"),
    menuentry("De/reattach",    "ioncore.detach(_, 'toggle')"), 
})


-- Context menu for workspaces
defctxmenu("WGroupWS", "Workspace", {
    menuentry("Close",          "WRegion.rqclose(_)"),
    menuentry("Rename",         "mod_query.query_renameworkspace(nil, _)"),
    menuentry("Attach tagged",  "ioncore.tagged_attach(_)"),
})


-- Context menu for client windows
defctxmenu("WClientWin", "Client window", {
    menuentry("Kill",           "WClientWin.kill(_)"),
})

