-- Preserve the configured method and path for all three v1 routes and the
-- shared v2 GET / and POST /user routes.
function init(args)
  local root = args[1] or "."
  local common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
end
