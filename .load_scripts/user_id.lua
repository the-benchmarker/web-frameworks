-- user_id.lua: a different id on every request, fixed width (8 digits)
local threads = 0
function setup(thread)
  threads = threads + 1
  thread:set("tid", threads)
end
function init(args)
  n = tid * 1000003 -- LuaJIT seeds every thread alike, so one stream per thread
end
function request()
  n = (n + 7919) % 10000000
  return wrk.format("GET", string.format("/user/%08d", n))
end
