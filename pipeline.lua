-- duration in microseconds
-- requests
-- connection errors
-- read errors
-- write errors
-- status errors
-- timeout errors
  
done = function(summary, latency, requests)

  out = {
    summary.duration,
    summary.requests,
    summary.errors.connect,
    summary.errors.read,
    summary.errors.write,
    summary.errors.status,
    summary.errors.timeout,
  }

  for key, value in pairs(out) do
    if key > 1 then
      io.stderr:write(",")
    end
    io.stderr:write(string.format("%d", value))
  end

end
