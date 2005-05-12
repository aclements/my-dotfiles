-- statusd_batacpi.lua
-- ACPI-based battery monitor for Ion3 statusd
-- 
-- Austin Clements <amdragon(at)mit(dot)edu>

-- Based on Bas Kok's script (see ion mailing list Oct 18, 2004)

local settings={
   interval=30*1000,		-- Milliseconds between updates
   battery=0,                   -- Battery number to read from /proc
   width=15,                    -- Width of the battery bar
   testing=false,               -- Set to true to run standalone
}

local timer = nil

local function read_proc_file(fname)
   local info = {}

   for line in io.lines(fname) do
      local _, _, field, value = string.find(line, '^([^:]*):%s*(.*)')
      if field then
         info[field] = value
      end
   end

   return info
end

local function get_number(str)
   local _, _, num = string.find(str, '^(%d*)')
   return tonumber(num)
end

local function get_battery_info()
   -- Reschedule myself
   if not settings.testing then
      timer:set(settings.interval, get_battery_info)
   end

   local dir = string.format('/proc/acpi/battery/BAT%d/', settings.battery)

   -- Read general battery info
   local info = read_proc_file(dir .. 'info')
   local capacity = get_number(info['last full capacity'])

   -- Read current state
   local state = read_proc_file(dir .. 'state')
   local charging = state['charging state']
   local rate = get_number(state['present rate'])
   local remaining = get_number(state['remaining capacity'])

   -- Compute percentage bar
   local percentage = math.min(math.max(100*remaining/capacity,0),100)
   local elem
   if charging == 'charging' then
      elem = '>>'
   elseif charging == 'discharging' then
      elem = '<<'
   elseif charging == 'charged' then
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
   if charging == 'charged' or rate == 0 then
      hours = 0
   elseif charging == 'charging' then
      hours = math.max(capacity-remaining, 0)/rate
   elseif charging == 'discharging' then
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
   local output
   output = string.format('%d%% %s %s', percentage, bar, time)

   if settings.testing then
      print(output)
   else
      statusd.inform("batacpi", output)
   end
end

-- Start the timer
if not settings.testing then
   timer = statusd.create_timer()
end
get_battery_info()
