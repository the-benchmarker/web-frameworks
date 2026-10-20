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

function common.fixture(root)
  local file = assert(io.open(root .. "/serialization.json", "rb"))
  local json = file:read("*a")
  file:close()
  return json
end

return common
