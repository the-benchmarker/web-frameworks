-- duration in microseconds
-- errors (Non-2xx or 3xx responses)
-- success (2xx responses)
  
done = function(summary, latency, requests)

  out = {
    summary.duration,
    summary.errors.connect,
    summary.requests,
  }

  for key, value in pairs(out) do
    if key > 1 then
      io.stderr:write(",")
    end
    io.stderr:write(string.format("%d", value))
  end

end
