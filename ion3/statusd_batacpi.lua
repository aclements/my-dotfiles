-- statusd_batacpi.lua
-- ACPI-based battery monitor for Ion3 statusd
-- 
-- Austin Clements <amdragon(at)mit(dot)edu>

-- Derived from Bas Kok's script (see ion mailing list Oct 18, 2004)

local defaults = {
   interval=30*1000,		-- Milliseconds between updates
   battery=0,                   -- Battery number to read from /proc
   width=15,                    -- Width of the battery bar
   critical_percent=10,         -- Critical battery percentage
   critical_fields="percent bar time", -- Fields to mark as critical
   testing=false,               -- Set to true to run standalone
}

local settings
if statusd then
   settings = table.join(statusd.get_config(me), defaults)
else
   settings = defaults
end

local me = "batacpi"

local timer = nil

local function setup_statusd()
   statusd.inform(me.."_percent_template", "100%")
   statusd.inform(me.."_bar_template",
                  ">"..string.rep('|',settings.width).."<")
   statusd.inform(me.."_time_tempate", "9:99")
   timer = statusd.create_timer()
end

local function read_proc_file(fname)
   local info = {}
   local success, lines = pcall(io.lines, fname)

   if not success then
      return nil
   end

   for line in lines do
      local _, _, field, value = string.find(line, '^([^:]*):%s*(.*)')
      if field then
         info[field] = value
      end
   end

   return info
end

local function read_sysfs(name)
   local fname = string.format('/sys/class/power_supply/BAT%d/%s',
                               settings.battery, name)
   local success, lines = pcall(io.lines, fname)

   if not success then
      return nil
   end

   for line in lines do
      return line
   end

   return nil
end

local function get_number(str)
   local _, _, num = string.find(str, '^(%d*)')
   return tonumber(num)
end

local function get_battery_info_procfs()
   -- Used to work, not tested since the procfs/sysfs split

   local dir = string.format('/proc/acpi/battery/BAT%d/', settings.battery)

   -- Read /proc info
   local info = read_proc_file(dir .. 'info')
   local state = read_proc_file(dir .. 'state')
   if not info or not state then
      return nil
   end

   -- Read current state
   local charging = state['charging state']
   if charging == 'charging' then
      charging = 1
   elseif charging == 'discharging' then
      charging = -1
   elseif charging == 'charged' then
      charging = 0
   else
      charging = nil
   end

   return {capacity  = get_number(info['last full capacity']),
           charging  = charging,
           rate      = get_number(state['present rate']),
           remaining = get_number(state['remaining capacity'])}
end

local function get_battery_info_sysfs()
   local capacity = read_sysfs('energy_full')
   local charging = read_sysfs('status')
   local rate = read_sysfs('current_now')
   local remaining = read_sysfs('energy_now')

   if not (capacity and charging and rate and remaining) then
      return nil
   end

   if charging == 'Charging' then
      charging = 1
   elseif charging == 'Discharging' then
      charging = -1
   elseif charging == 'Not charging' or charging == 'Full' then
      charging = 0
   else
      -- XXX
      if statusd then
         statusd.warn('Unknown charge status '..charging)
      end
      charging = nil
   end

   return {capacity  = get_number(capacity),
           charging  = charging,
           rate      = get_number(rate),
           remaining = get_number(remaining)}
end

local function get_battery_info()
   info = get_battery_info_procfs()
   if not info then
      info = get_battery_info_sysfs()
   end
   if not info then
      statusd.warn("Failed to read battery info files")
      timer:set(settings.interval/1000, get_battery_info)
      return
   end
   
   -- Reschedule myself
   if not settings.testing then
      timer:set(settings.interval, get_battery_info)
   end

   remaining = info.remaining
   capacity = info.capacity
   charging = info.charging
   rate = info.rate

   -- Compute percentage
   local percentage = math.min(math.max(100*remaining/capacity,0),100)

   -- Compute percentage bar
   local elem
   if charging == 1 then
      elem = '>>'
   elseif charging == -1 then
      elem = '<<'
   elseif charging == 0 then
      elem = '><'
   else
      elem = '??'
   end
   local bar = string.sub(elem,1,1)
   bar = bar .. string.rep('|', percentage*settings.width/100)
   bar = bar .. string.rep(' ', settings.width - percentage*settings.width/100)
   bar = bar .. string.sub(elem,2,2)

   -- Compute remaining time
   local hours
   if charging == 0 or rate == 0 then
      hours = 0
   elseif charging == 1 then
      hours = math.max(capacity-remaining, 0)/rate
   elseif charging == -1 then
      hours = remaining/rate
   else
      hours = -1
   end

   local minutes = math.floor(math.mod(hours, 1)*60)
   hours = math.floor(hours)

   local time
   if hours == -1 then
      time = "?:??"
   elseif hours == 0 and minutes == 0 then
      time = ""
   else
      time = string.format('%d:%.2d', hours, minutes)
   end

   -- Put it all together
   local percent = string.format("%d%%", percentage)

   if settings.testing then
      print(percent.." "..bar.." "..time)
   else
      statusd.inform(me.."_percent", percent)
      statusd.inform(me.."_bar", bar)
      statusd.inform(me.."_time", time)
      for field in string.gfind(settings.critical_fields, "%a+") do
         if percentage <= settings.critical_percent then
            statusd.inform(me.."_"..field.."_hint", "critical")
         else
            statusd.inform(me.."_"..field.."_hint", "normal")
         end
      end
   end
end

function check_batacpi()
   if get_battery_info_procfs() or get_battery_info_sysfs() then
      return true
   else
      return false
   end
end

if statusd or settings.testing then
   -- Start the timer
   if not settings.testing then
      setup_statusd()
   end
   get_battery_info()
end
