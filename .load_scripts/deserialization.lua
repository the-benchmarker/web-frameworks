local bodies = {}
local number = 0

function init(args)
  local root = args[1] or "."
  local common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
  wrk.headers["Content-Type"] = "application/json"
  local fixture = common.fixture(root)

  -- Prepare equal-sized requests before load starts. Every variant has 100
  -- three-digit IDs, so changing the body never changes the request size.
  for group = 1, 9 do
    local id = group * 100
    local json, count = fixture:gsub('("id"%s*:%s*)%d+', function(prefix)
      local value = prefix .. id
      id = id + 1
      return value
    end)
    assert(count == 100, "serialization.json must contain exactly 100 items")
    bodies[group] = '{"items":' .. json .. '}'
    assert(#bodies[group] == #bodies[1], "request body sizes must match")
  end
end

function request()
  number = number % #bodies + 1
  return wrk.format("POST", "/deserialization", wrk.headers, bodies[number])
end
