local common
local number = 0
local prefix = "--benchmark-boundary\r\n"
  .. "Content-Disposition: form-data; name=\"seed\"\r\n\r\n"
local file = "\r\n--benchmark-boundary\r\n"
  .. "Content-Disposition: form-data; name=\"file\"; filename=\"payload.bin\"\r\n"
  .. "Content-Type: application/octet-stream\r\n\r\n"
  .. string.rep("abcd", 1024)
  .. "\r\n--benchmark-boundary--\r\n"

function init(args)
  local root = args[1] or "."
  common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
  wrk.headers["Content-Type"] = "multipart/form-data; boundary=benchmark-boundary"
end

function request()
  number = (number + 1) % 208827064576 -- 26^8
  return wrk.format("POST", "/upload", wrk.headers, prefix .. common.seed(number) .. file)
end
