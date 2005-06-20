-- The frame control bindings are loosely based on Emacs

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
		      "WIonWS.goto_dir_nowrap(_, 'above')"),
	       kpress(MOD1.."Down",
		      "WIonWS.goto_dir_nowrap(_, 'below')"),
	       kpress(MOD1.."Right",
		      "WIonWS.goto_dir_nowrap(_, 'right')"),
	       kpress(MOD1.."Left",
		      "WIonWS.goto_dir_nowrap(_, 'left')"),

	       bdoc("Go to frame above/below/right/left with alphabetic keys"),
	       kpress(MOD1.."h",
		      "WIonWS.goto_dir_nowrap(_, 'left')"),
	       kpress(MOD1.."j",
		      "WIonWS.goto_dir_nowrap(_, 'below')"),
	       kpress(MOD1.."k",
		      "WIonWS.goto_dir_nowrap(_, 'above')"),
	       kpress(MOD1.."l",
		      "WIonWS.goto_dir_nowrap(_, 'right')"),
	    })
