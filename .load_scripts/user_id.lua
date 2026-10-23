-- A different path value on every request, always eight digits wide.
local n = 0
local threads = 0
function setup(thread)
  threads = threads + 1
  thread:set("tid", threads)
end
function init(args)
  local root = args[1] or "."
  local common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
  n = (tid or 0) * 1000003
end
function request()
  n = (n + 7919) % 10000000
  return wrk.format("GET", string.format("/user/%08d", n))
end
