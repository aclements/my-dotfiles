--
-- Menu definitions
--


-- Main menu
defmenu("mainmenu", {
    submenu("Programs",         "appmenu"),
    menuentry("Lock screen",    "ioncore.exec_on(_, 'xlock')"),
    menuentry("Help",           "mod_query.query_man(_)"),
    menuentry("About Ion",      "mod_query.show_about_ion(_)"),
    submenu("Styles",           "stylemenu"),
    submenu("Session",          "sessionmenu"),
})


-- Application menu
defmenu("appmenu", {
    menuentry("XTerm",          "ioncore.exec_on(_, 'xterm')"),
    menuentry("W3M",            "ioncore.exec_on(_, ':w3m -v')"),
    menuentry("Rxvt",           "ioncore.exec_on(_, 'rxvt')"),
    menuentry("Opera",          "ioncore.exec_on(_, 'opera')"),
    menuentry("Links",          "ioncore.exec_on(_, ':links')"),
    menuentry("Konqueror",      "ioncore.exec_on(_, 'konqueror')"),
    menuentry("Dillo",          "ioncore.exec_on(_, 'dillo')"),
    menuentry("Run...",         "mod_query.query_exec(_)"),
})


-- Session control menu
defmenu("sessionmenu", {
    menuentry("Save",           "ioncore.snapshot()"),
    menuentry("Restart",        "ioncore.restart()"),
    menuentry("Restart TWM",    "ioncore.restart_other('twm')"),
    menuentry("Exit",           "ioncore.shutdown()"),
})


-- Context menu (frame/client window actions)
defctxmenu("WFrame", "Frame", {
    menuentry("Close",          "WRegion.rqclose_propagate(_, _sub)"),
    menuentry("Kill",           "WClientWin.kill(_sub)",
                                "_sub:WClientWin"),
    menuentry("Toggle tag",     "WRegion.set_tagged(_sub, 'toggle')",
                                "_sub:non-nil"),
    menuentry("Attach tagged",  "WFrame.attach_tagged(_)"),
    menuentry("Clear tags",     "ioncore.clear_tags()"),
    menuentry("Window info",    "mod_query.show_clientwin(_, _sub)",
                                "_sub:WClientWin"),
})
