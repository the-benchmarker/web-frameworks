done = function(summary, latency, requests)
  file = io.open('/tmp/which_is_the_fastest.out', 'w')
  file:write(string.format("%d,%d,%d\n", latency.min, latency.max, latency.mean))
  file.close(file)
end
