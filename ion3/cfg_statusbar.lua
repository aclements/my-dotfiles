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
if trydopath("statusd_wireless.lua") and check_wireless("eth1") then
   wireless = sep ..
     "%>wireless_eth1_level %|wireless_eth1_bar %wireless_eth1_essid"
end

-- Embed docker in the system tray.  Without this, docker is placed in
-- the status bar, but uses the standard 64x64 WM dockapp dimensions.
defwinprop {
   class = "Docker",
   statusbar = "dock",
   -- XXX Without this it's 2 pixels wide, but it probably won't
   -- resize for new icons
   min_size = {w=24,h=24}
}

-- Create primary statusbar
mod_statusbar.create{
   -- First screen, bottom left corner
   screen=0,
   pos='bl',
   fullsize=false,
   systray=true,

   -- Template
   template="%date"..sep.."%load"..batacpi..wireless.."%filler%systray%systray_dock",
}

-- Launch ion-statusd.  This must be done after creating statusbars
mod_statusbar.launch_statusd{
   date={
      -- Standard ISO-8601 date format with abbreviated day name
      date_format='%a %m/%d %l:%M %P',
   }      
}
