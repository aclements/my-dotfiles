-- Statusbar module

-- Create primary statusbar
mod_statusbar.create{
   -- First screen, bottom left corner
   screen=0,
   pos='bl',
   fullsize=false,
   systray=true,

   -- Template
   template="%date  :.  %load  :.  "..
      "%>batacpi_percent %|batacpi_bar %>batacpi_time  :.  "..
      "%>wireless_eth1_level %|wireless_eth1_bar %wireless_eth1_essid"..
      "%filler%systray",
}

-- Launch ion-statusd.  This must be done after creating statusbars
mod_statusbar.launch_statusd{
   date={
      -- Standard ISO-8601 date format with abbreviated day name
      date_format='%a %m/%d %l:%M %P',
   }      
}
