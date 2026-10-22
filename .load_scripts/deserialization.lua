local fragments = {}
local next_token
local threads = 0

function setup(thread)
  threads = threads + 1
  thread:set("tid", threads)
end

function init(args)
  local root = args[1] or "."
  local common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
  wrk.headers["Content-Type"] = "application/json"
  next_token = common.sequence(tid)

  -- JSON numbers cannot have leading zeros. Keep every ID six digits wide,
  -- including the IDs that do not change, without changing the other fields.
  local body, count = common.fixture(root, "deserialization.json"):gsub('("id"%s*:%s*)(%d+)', function(prefix, id)
    local value = 100000 + tonumber(id)
    assert(value <= 999999, "fixture ID exceeds the six-digit load range")
    return prefix .. string.format("%06d", value)
  end)
  assert(count == 100, "expected 100 fixture IDs")

  -- Split once so request() only replaces the five token fields.
  local cursor = 1
  for index = 1, 5 do
    local _, _, first, after = body:find('"id"%s*:%s*()%d+()', cursor)
    fragments[index * 2 - 1] = body:sub(cursor, first - 1)
    fragments[index * 2] = ""
    cursor = after
  end
  fragments[11] = body:sub(cursor)
end

function request()
  local token = next_token()
  for index = 1, 5 do
    fragments[index * 2] = token:sub(index * 6 - 5, index * 6)
  end
  return wrk.format("POST", "/deserialization", wrk.headers, table.concat(fragments))
end
