-- Ion3 bindings
-- By Austin Clements <amdragon@mit.edu>

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

	       bdoc("Display the main menu."),
	       kpress(MOD2.."F12", "mod_menu.bigmenu(_, _sub, 'mainmenu')"),
	       mpress("Button3", "mod_menu.pmenu(_, _sub, 'mainmenu')"),
	    })

function WClientWin.smart_fullscreen(window)
   local scr = window:screen_of()
   local ws = ioncore.lookup_region("Fullscreen", "WIonWS")
   if not ws then
      ws = scr:attach_new({type="WIonWS",
                             name="Fullscreen",
                             index=scr:lcount(1)})
   end
   ws:current():attach(window, {switchto=true})
   ws:goto()
end

defbindings("WMPlex", {
               bdoc("Quote the next key press"),
               kpress(MOD1.."Q",
                      "WClientWin.quote_next(_sub)", "_sub:WClientWin"),
               
	       bdoc("Nudge current client window. This might help with some "..
		    "programs' resizing problems."),
	       kpress_wait(MOD1.."P", 
			   "WClientWin.nudge(_sub)", "_sub:WClientWin"),

	       bdoc("Toggle fullscreen mode of current client window."),
               kpress(MOD1.."1",
                      "WClientWin.smart_fullscreen(_sub)", "_sub:WClientWin"),
-- 	       kpress_wait(MOD1.."1", 
-- 			   "WClientWin.set_fullscreen(_sub, 'toggle')", 
-- 			   "_sub:WClientWin"),

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

	       bdoc("Query for workspace to go to or create a new one."),
	       kpress(MOD2.."F9", "mod_query.query_workspace(_)"),
	    })

defbindings("WFrame", {
	       bdoc("Tag current object within the frame."),
	       kpress(MOD1.."T", "WRegion.set_tagged(_sub, 'toggle')",
                      "_sub:non-nil"),

	       bdoc("Switch to next/previous object within the frame."),
	       kpress(MOD1.."I", "WFrame.switch_next(_)"),
	       kpress(MOD1.."U", "WFrame.switch_prev(_)"),

	       bdoc("Maximize the frame vertically."),
	       kpress(MOD1.."V", "WFrame.maximize_vert(_)"),

	       bdoc("Attach tagged objects to this frame."),
	       kpress(MOD1.."A", "WFrame.attach_tagged(_)"),

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
