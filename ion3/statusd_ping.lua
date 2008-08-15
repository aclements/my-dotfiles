-- statusd_ping.lua

local timer = nil

--statusd.inform("ping_template", "000.000.000.000")

local laststr = nil
local lasthint = nil
local function inform(str, hint)
   if str ~= laststr or hint ~= lasthint then
      --print(str.." ("..hint..")")
      statusd.inform("ping", str)
      statusd.inform("ping_hint", hint)
      laststr = str
      lasthint = hint
   end
end

local function warn(msg)
   --print("Warning: "..str)
   statusd.warn(msg)
end

local lasthostcheck = nil
local host = nil
local function gethost()
   local now = os.time()
   if lasthostcheck == nil or os.difftime(now, lasthostcheck) > 10 then
      local f, res = io.open("/mts/home1/aclements/.defaulthost", "r")
      if not f then
         warn(res)
         return false
      end
      host = f:read("*l")
   end
   return true
end

local function process(str)
   -- Skip header
   local buf = ""
   while str do
      buf = buf .. str
      local nlpos = string.find(buf, "\n", 1, true)
      if nlpos then
         buf = string.sub(buf, nlpos + 1)
         break
      end
      str = coroutine.yield()
   end
   -- Read ping output
   str = buf
   local count = 0
   while str do
      for i = 1, string.len(str) do
         local ch = string.sub(str, i, i)
         if ch == " " then
            count = 0
         elseif ch == "." or ch == "E" then
            count = count + 1
         elseif ch == "\b" then
            -- Should be followed by " " or "E"
         else
            warn("Unknown ping output: \"" .. ch .. "\"")
         end
      end

      if count >= 5 then
         inform(host, "critical")
      elseif count >= 3 then
         inform(host, "important")
      else
         inform(host, "normal")
      end

      str = coroutine.yield()
   end
   -- Ping quit
   warn("Ping exited unexpectedly")
   -- XXX Restart ping
end

gethost()
statusd.popen_bgread("ping -f -i 1 "..host,
                     coroutine.wrap(process),
                     coroutine.wrap(process))


-- local ping = nil
-- local count = 0
-- while true do
--    if not gethost() then
--       break
--    end
--    if ping == nil then
--       local success
--       success, ping = pcall(io.popen, "ping -f -i 1 "..host)
--       if not success then
--          warn("Failed to start ping")
--          break
--       end
--       -- Header
--       ping:read("*l")
--    end

--    ch = ping:read(1)
--    if ch == nil then
--       warn("Ping exited unexpectedly")
--       -- Restart ping
--       ping = nil
--    elseif ch == " " then
--       count = 0
--    elseif ch == "." or ch == "E" then
--       count = count + 1
--    elseif ch == "\b" then
--       -- Should be followed by " " or "E"
--    else
--       warn("Unknown ping output: \"" .. ch .. "\"")
--    end
--    if count >= 5 then
--       inform(host, "critical")
--    elseif count >= 3 then
--       inform(host, "important")
--    else
--       inform(host, "normal")
--    end
-- end
