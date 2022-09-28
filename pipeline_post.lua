wrk.method = "POST"
wrk.body = ""
wrk.headers["Content-Type"] = "application/x-www-form-urlencoded"

-- duration in microseconds
-- total completed requests
-- total completed requests per seconds
-- total bytes received
-- total socket connection errors
-- total socket read errors
-- total socket write errors
-- total http errors (status > 399)
-- total request timeouts
-- minimum latency
-- maximum latency
-- average latency
-- standard deviation
-- percentile : 50
-- percentile : 75
-- percentile : 90
-- percentile : 99
-- percentile : 99.999
  
done = function(summary, latency, requests)

  out = {
    summary.duration,
    summary.requests,
    summary.requests/(summary.duration/1000000),
    summary.bytes,
    summary.errors.connect,
    summary.errors.read,
    summary.errors.write,
    summary.errors.status,
    summary.errors.timeout,
    latency.min,
    latency.max,
    latency.mean,
    latency.stdev,
    latency:percentile(50),
    latency:percentile(75),
    latency:percentile(90),
    latency:percentile(99),
    latency:percentile(99.999)
  }

  for key, value in pairs(out) do
    if key > 1 then
      io.stderr:write(",")
    end
    io.stderr:write(string.format("%d", value))
  end

end
