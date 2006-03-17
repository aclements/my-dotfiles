-- statusd_wireless.lua
-- Wireless monitor for Ion3 statusd
-- 
-- Austin Clements <amdragon(at)mit(dot)edu>

local defaults = {
   interval=3*1000,		-- Milliseconds between updates
   interfaces="eth1",           -- Interfaces to report status for
   width=15,                    -- Width of the link quality bar
   critical_level=60,           -- Critical low-quality level
   critical_fields="level bar essid", -- Fields to mark as critical
   no_lease_color="important",  -- Color to use when an iface has no lease
   testing=false,               -- Set to true to run standalone
}

local me = "wireless"

local settings = defaults
if not settings.testing then
   settings = table.join(statusd.get_config(me), settings)
end

local timer = nil

local function warn(msg)
   if settings.testing then
      print("Warning: "..msg)
   else
      statusd.warn(msg)
   end
end

local function get_interfaces()
   return string.gfind(settings.interfaces, "%w+")
end

local function setup_statusd()
   for iface in get_interfaces() do
      statusd.inform(me.."_"..iface.."_level_template", "100")
      statusd.inform(me.."_"..iface.."_bar_template",
                     ">"..string.rep('|',settings.width).."<")
      statusd.inform(me.."_".."_essid_tempate", "mmmmmmmmm")
   end
   timer = statusd.create_timer()
end

local function read_proc_file()
   local levels = {}
   local lines = io.lines('/proc/net/wireless')

   if not lines then
      return nil
   end

   for line in lines do
      local _, _, iface, level = string.find(line, '^  (%w*):%s*%d*%s*(%d*)')
      if iface then
         levels[iface] = tonumber(level)
      end
   end

   return levels
end

local function get_iface_essid(iface)
   local essid = io.popen("/usr/sbin/iwgetid "..iface.." --raw")

   if not essid then
      warn("Failed to read essid for "..iface)
      return ""
   end

   for line in essid:lines() do
      return line
   end

   return "No essid"
end

local function iface_has_lease(iface)
   local lines = io.lines('/proc/net/route')

   if not lines then
      return false
   end

   for line in lines do
      local _, _, thisiface = string.find(line, "^(%w*)%s*%d*")
      if thisiface and thisiface == iface then
         return true
      end
   end
   return false
end

local function inform(iface, level, bar, essid, hint)
   if settings.testing then
      print(iface..": "..level.." "..bar.." "..essid.." ("..hint..")")
   else
      statusd.inform(me.."_"..iface.."_level", level)
      statusd.inform(me.."_"..iface.."_bar", bar)
      statusd.inform(me.."_"..iface.."_essid", essid)

      for field in string.gfind(settings.critical_fields, "%w+") do
         statusd.inform(me.."_"..iface.."_"..field.."_hint", hint)
      end
   end
end

local function get_wireless_info()
   -- Read levels
   local levels = read_proc_file()
   if not levels then
      warn("Failed to read wireless file")
      if not settings.testing then
         timer:set(settings.interval/1000, get_wireless_info)
      end
      return
   end
   
   -- Reschedule myself
   if not settings.testing then
      timer:set(settings.interval, get_wireless_info)
   end

   -- Iterate over interfaces
   for iface in get_interfaces() do
      -- Read interface info
      local level = levels[iface]
      local essid = get_iface_essid(iface)

      if not level or not essid then
         inform(iface, "?", "?", "No "..iface, "critical")
      else
         -- Compute bar
         local bar = ""
         if level*settings.width/100 > 0 then
            bar = bar .. string.rep('|', level*settings.width/100)
            bar = bar .. string.rep(' ',
                                    settings.width
                                       - level*settings.width/100)
         else
            -- The status bar will remove the field if it's all spaces
            bar = bar .. ":" .. string.rep(' ', settings.width - 1)
         end

         -- Compute hint
         local hint = "normal"
         if not iface_has_lease(iface) then
            hint = settings.no_lease_color
         elseif level <= settings.critical_level then
            hint = "critical"
         end

         -- Put it all together
         local levelStr = string.format("%d", level)

         inform(iface, levelStr, bar, essid, hint)
      end
   end
end

-- Start the timer
if not settings.testing then
   setup_statusd()
end
get_wireless_info()
