done = function(summary, latency, requests)
  file = io.open('/tmp/which_is_the_fastest.out', 'w')
  file:write(string.format("%d\n", summary.errors.connect))
  file.close(file)
end
