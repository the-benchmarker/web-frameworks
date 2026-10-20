local common
local number = 0

function init(args)
  local root = args[1] or "."
  common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
end

function request()
  number = (number + 1) % 208827064576 -- 26^8
  return wrk.format("GET", "/serialization?seed=" .. common.seed(number))
end
