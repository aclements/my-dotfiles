-- statusd_ping.lua

local timer = nil

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

local host = nil
local pingpid = nil
local restartping = nil

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
   pingpid = nil
   if restartping then
      startping(restartping)
      restartping = nil
   else
      warn("Ping exited unexpectedly")
      -- Restart ping on the next gethost
      host = nil
   end
end

function startping(newhost)
   if pingpid then
      restartping = newhost
      pcall(os.execute, "kill "..pingpid)
   else
      host = newhost
      pingpid = statusd.popen_bgread("exec ping -f -i 1 "..host,
                                     coroutine.wrap(process),
                                     coroutine.wrap(process))
   end
end

local function gethost()
   local f, res = io.open("/mts/home1/aclements/.defaulthost", "r")
   if not f then
      inform("No .defaulthost", "important")
   else
      local newhost = f:read("*l")
      if newhost ~= host then
         startping(newhost)
      end
   end
   timer:set(10*1000, gethost)
end

timer = statusd.create_timer()
gethost()
