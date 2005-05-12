mod_statusbar.set{
   date_format='%a %m/%e %l:%M %P',
   template="%date :. %batacpi",
}

-- Create a statusbar
mod_statusbar.create{
    -- First screen, bottom left corner
    screen=0,
    pos='bl',
}
