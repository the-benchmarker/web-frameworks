-- Inspired by https://github.com/TechEmpower/FrameworkBenchmarks/issues/2960

done = function(summary, latency, requests)
  avg = latency.mean
  max = latency.max
  min = latency.min
  req = summary.requests
  time = summary.duration
  file = io.open('/tmp/which_is_the_fastest.out', 'w')
  file:write(string.format("%d,%d,%d,%d,%d\n", time, avg, max, min, req))
  file.close(file)
end
