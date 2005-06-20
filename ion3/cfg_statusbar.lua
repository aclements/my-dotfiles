-- Statusbar module

-- Create primary statusbar
mod_statusbar.create{
   -- First screen, bottom left corner
   screen=0,
   pos='bl',

   -- Standard ISO-8601 date format with abbreviated day name
   date_format='%a %m/%e %l:%M %P',

   -- Template
   -- XXX Grr.  I want this whole thing centered
   template="%date  :.  %load  :.  "..
      "%>batacpi_percent %|batacpi_bar %>batacpi_time",
}

-- Launch ion-statusd.  This must be done after creating statusbars
mod_statusbar.launch_statusd{}
