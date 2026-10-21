local body

function init(args)
  local root = args[1] or "."
  local common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
  wrk.headers["Content-Type"] = "application/json"
  body = common.fixture(root, "deserialization.json")
end

function request()
  return wrk.format("POST", "/deserialization", wrk.headers, body)
end
