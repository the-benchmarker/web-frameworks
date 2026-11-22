local suffix
local next_token
local threads = 0
local prefix = "--benchmark-boundary\r\n"
  .. "Content-Disposition: form-data; name=\"file\"; filename=\"payload.bin\"\r\n"
  .. "Content-Type: application/octet-stream\r\n\r\n"

function setup(thread)
  threads = threads + 1
  thread:set("tid", threads)
end

function init(args)
  local root = args[1] or "."
  local common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
  next_token = common.sequence(tid)
  local fixture = common.fixture(root, "upload.bin")
  assert(#fixture == 4096, "expected a 4096-byte upload fixture")
  -- Expand once to 256 KiB. Each request replaces the first 30 file bytes;
  -- the file length and multipart framing stay constant.
  suffix = fixture:rep(64):sub(31) .. "\r\n--benchmark-boundary--\r\n"
  wrk.headers["Content-Type"] = "multipart/form-data; boundary=benchmark-boundary"
end

function request()
  return wrk.format("POST", "/upload", wrk.headers, prefix .. next_token() .. suffix)
end
