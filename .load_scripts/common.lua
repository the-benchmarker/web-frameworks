local common = {}

function common.configure()
  wrk.headers = wrk.headers or {}
  wrk.headers["Accept-Encoding"] = "identity"
end

function common.seed(number)
  local characters = {}
  for index = 8, 1, -1 do
    characters[index] = string.char(97 + number % 26)
    number = math.floor(number / 26)
  end
  return table.concat(characters)
end

function common.fixture(root, name)
  local file = assert(io.open(root .. "/" .. name, "rb"))
  local body = file:read("*a")
  file:close()
  return body
end

-- Five six-digit numbers: two random run fields, a thread ID, and a two-field
-- counter. The same token fits numeric JSON IDs and a fixed-width upload marker.
function common.sequence(thread_id)
  local radix = 900000
  assert(thread_id and thread_id >= 1 and thread_id < radix, "invalid load thread ID")
  local file = assert(io.open("/dev/urandom", "rb"))
  local bytes = assert(file:read(8))
  file:close()
  assert(#bytes == 8, "could not read load sequence randomness")

  local function random_field(offset)
    local a, b, c, d = bytes:byte(offset, offset + 3)
    return 100000 + (((a * 256 + b) * 256 + c) * 256 + d) % radix
  end

  local prefix = string.format("%06d%06d%06d", random_field(1), random_field(5), 100000 + thread_id)
  local counter = 0
  return function()
    counter = counter + 1
    assert(counter < radix * radix, "load sequence exhausted")
    return prefix .. string.format("%06d%06d", 100000 + math.floor(counter / radix), 100000 + counter % radix)
  end
end

return common
