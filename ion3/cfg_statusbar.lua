-- Statusbar module

local sep = "  :.  "

local function trydopath(name)
   local path = ioncore.lookup_script(name)
   if path then
      dofile(path)
      return true
   else
      return false
   end
end

local batacpi = ""
if trydopath("statusd_batacpi.lua") and check_batacpi() then
   batacpi = sep ..
      "%>batacpi_percent %|batacpi_bar %>batacpi_time"
end

local wireless = ""
if trydopath("statusd_wireless.lua") and check_wireless() then
   wireless = sep ..
     "%>wireless_eth1_level %|wireless_eth1_bar %wireless_eth1_essid"
end

-- Create primary statusbar
mod_statusbar.create{
   -- First screen, bottom left corner
   screen=0,
   pos='bl',
   fullsize=false,
   systray=true,

   -- Template
   template="%date"..sep.."%load"..batacpi..wireless.."%filler%systray",
}

-- Launch ion-statusd.  This must be done after creating statusbars
mod_statusbar.launch_statusd{
   date={
      -- Standard ISO-8601 date format with abbreviated day name
      date_format='%a %m/%d %l:%M %P',
   }      
}
