function init(args)
  local root = args[1] or "."
  local common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
end
