local body
local prefix = "--benchmark-boundary\r\n"
  .. "Content-Disposition: form-data; name=\"file\"; filename=\"payload.bin\"\r\n"
  .. "Content-Type: application/octet-stream\r\n\r\n"

function init(args)
  local root = args[1] or "."
  local common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
  body = prefix .. common.fixture(root, "upload.bin") .. "\r\n--benchmark-boundary--\r\n"
  wrk.headers["Content-Type"] = "multipart/form-data; boundary=benchmark-boundary"
end

function request()
  return wrk.format("POST", "/upload", wrk.headers, body)
end
