local common
local number = 0
local threads = 0
local offset = 0

-- Eight lowercase letters: a two-letter thread prefix and a six-letter counter.
-- This gives 676 threads disjoint ranges of 308,915,776 seeds each per run.
-- Seeds repeat between runs; this scheme guarantees uniqueness within one run.
local max_threads = 26 ^ 2
local seeds_per_thread = 26 ^ 6

function setup(thread)
  threads = threads + 1
  assert(threads <= max_threads, "serialization supports at most 676 load threads")
  thread:set("tid", threads)
end

function init(args)
  local root = args[1] or "."
  common = dofile(root .. "/.load_scripts/common.lua")
  common.configure()
  assert(tid and tid >= 1 and tid <= max_threads and tid == math.floor(tid),
    "invalid serialization load thread ID")
  -- common.seed encodes base 26: this offset fixes the first two letters.
  offset = (tid - 1) * seeds_per_thread
  number = 0
end

function request()
  -- Fail instead of wrapping or spilling into another thread's prefix.
  assert(number < seeds_per_thread, "serialization seed sequence exhausted")
  local seed = common.seed(offset + number)
  number = number + 1
  return wrk.format("GET", "/serialization?seed=" .. seed)
end
