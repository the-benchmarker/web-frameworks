#!env elixir


{opts, file_matchers, _invalid_args} =
  System.argv()
  |> OptionParser.parse(
    strict: [
      help: :boolean,
      threads: :integer,
      connections: :integer,
      duration: :integer,
      warmup: :integer,
      verbose: :boolean,
      server: :string,
      sshpath: :string,
    ],
    aliases: [
      h: :help,
      "?": :help,
      t: :threads,
      c: :connections,
      d: :duration,
      w: :warmup,
      v: :verbose,
      s: :server,
      p: :sshpath,
    ]
  )


system_cores = :erlang.system_info(:logical_processors_available)
requested_cores = trunc(system_cores * (2/3))
requested_cores = if(requested_cores < 1, do: 1, else: requested_cores)

opts = %{
  help: opts[:help],
  threads: opts[:threads] || requested_cores,
  connections: opts[:connections] || 1000,
  duration: opts[:duration] || 5,
  warmup: opts[:warmup] || 5,
  verbose: opts[:verbose] || false,
  server: opts[:server] || "127.0.0.1",
  sshpath: opts[:sshpath],
}


urls = [
  {fn _ -> "http://#{opts.server}:3000/" end, nil},
  {fn a -> "http://#{opts.server}:3000/user/#{a}" end, nil},
  {fn _ -> "http://#{opts.server}:3000/user" end, "post"},
]


defmodule Helpers do
  def flush(label \\ nil) do
    receive do
      {port, {:data, output}} when is_port(port) ->
        if label, do: IO.puts("#{label}#{output}")
        flush(label)
      m ->
        if label, do: IO.puts("#{label}#{inspect(m)}")
        flush(label)
    after 0 -> :ok
    end
  end

  def server_run(path, opts) do
    if opts[:sshpath] do
      IO.inspect("ssh #{opts[:server]} -- bash #{opts[:sshpath]}/tools/_stats.sh #{opts[:sshpath]}/#{path}")
      Port.open({:spawn, "ssh #{opts[:server]} -- bash #{opts[:sshpath]}/tools/_stats.sh #{opts[:sshpath]}/#{path}"}, [:binary, :stderr_to_stdout ])
    else
      Port.open({:spawn, "bash tools/_stats.sh #{path}"}, [:binary, :stderr_to_stdout ])
    end
  end

  def parse_stat_format(str) do
    case String.split(str, "\n", trim: true) do
      [
        _doing,
        _specs,
        _headers,
        "    Latency "<>latency,
        "    Req/Sec "<>req_sec,
        _lat_dist_header,
        perc50,
        perc75,
        perc90,
        perc99,
        requests,
        errors,
        tot_req_sec,
        tot_transfer_sec,
      ] -> {latency, req_sec, perc50, perc75, perc90, perc99, requests, errors, tot_req_sec, tot_transfer_sec}
      [
        _doing,
        _specs,
        _headers,
        "    Latency "<>latency,
        "    Req/Sec "<>req_sec,
        _lat_dist_header,
        perc50,
        perc75,
        perc90,
        perc99,
        requests,
        tot_req_sec,
        tot_transfer_sec,
      ] -> {latency, req_sec, perc50, perc75, perc90, perc99, requests, "0", tot_req_sec, tot_transfer_sec}
      _ -> :error
    end
    |> case do
      :error -> :error
      {latency, req_sec, perc50, perc75, perc90, perc99, requests, errors, tot_req_sec, tot_transfer_sec} ->
        %{
          latency:
            case String.split(latency, " ", trim: true) do
              [avg, stdev, max, pmstdev] -> %{avg: avg, stdev: stdev, max: max, pmstdev: pmstdev}
              _ -> :parse_error
            end,
          req_sec:
            case String.split(req_sec, " ", trim: true) do
              [avg, stdev, max, pmstdev] -> %{avg: avg, stdev: stdev, max: max, pmstdev: pmstdev}
              _ -> :parse_error
            end,
          perc50:
            case String.split(perc50, " ", trim: true) do
              [_, value] -> value
              _ -> :parse_error
            end,
          perc75:
            case String.split(perc75, " ", trim: true) do
              [_, value] -> value
              _ -> :parse_error
            end,
          perc90:
            case String.split(perc90, " ", trim: true) do
              [_, value] -> value
              _ -> :parse_error
            end,
          perc99:
            case String.split(perc99, " ", trim: true) do
              [_, value] -> value
              _ -> :parse_error
            end,
          requests:
            case String.split(requests, " ", trim: true) do
              [requests, _request, _in, time, read, _read] ->
                %{requests: requests, time: String.replace(time, ",", ""), read: read}
              _ -> :parse_error
            end,
          errors:
            case Integer.parse(List.last(String.split(errors, " ", trim: true))) do
              {value, ""} -> value
              _ -> :parse_error
            end,
          tot_req_sec:
            case Float.parse(List.last(String.split(tot_req_sec, " ", trim: true))) do
              {value, ""} -> value
              _ -> :parse_error
            end,
          tot_transfer_sec: List.last(String.split(tot_transfer_sec, " ", trim: true)),
        }
    end
  end
end


if opts[:help] || file_matchers == [] do
  IO.puts(~S"""

  Usage:  tools/stats.exs <options> <matching-servers>

  Options are as listed below, matching-servers will run any server that even partially matches, thus `fastht` will match `go_fasthttprouter`.  For all built servers you can pass in just `_` as all servers contain that character.

  Examples:

    * tools/stats.exs cpp router_cr nim rust fasthttp
    * tools/stats.exs -w 1 -d 3 cpp crystal nim rust go

  Standard Options:

    --help, -h, -?                        : Show this help menu
    --threads <pos_int>, -t <pos_int>     : Number of threads to use, defaults: CPU Core count * 2/3 truncated
    --connections <pos_int>, -c <pos_int> : Max number of simultaneous streams to use, default: 1000
    --duration <pos_int>, -d <pos_int>    : Number of seconds per test, default: 5
    --warmup <pos_int>, -w <pos_int>      : Number of seconds to perform warmup, matters on GC languages, default: 5
    --verbose, -v                         : Turn on verbose logging


  SSH Options:

  Do note that currently the system should be built remotely and same-named (though empty is fine) server files should be on the client side as well.  Generally if the remote system type is the same as the local system then just build locally and upload the entire directory over.
  (TODO: Get server list from remote connection instead of getting the list locally)

    --server <string>, -s <string>        : The IP or DNS name of the remote server to test against and SSH to.
    --sshpath <string>, -p <string>       : The full path to the root location of the repo directory
  """)
  System.stop(0)
else



server_paths =
  file_matchers
  |> Enum.map(fn matcher ->
    Path.wildcard("bin/server_*#{matcher}*")
  end)
  |> List.flatten()

IO.puts("Total Cores: #{system_cores}")
IO.puts("Concurrent Connections: #{opts.connections}")
IO.puts("Threads: #{opts.threads}")
IO.puts("Warmup: #{opts.warmup} seconds")
IO.puts("Duration: #{opts.duration} seconds")
opts.verbose && IO.puts("Verbose: true")
opts.server != "127.0.0.1" && IO.puts("Server: #{opts.server}")
opts.sshpath && IO.puts("SSHPath: #{opts.sshpath}")
IO.puts("\nProcessing servers:\n* #{Enum.join(server_paths, "\n* ")}\n")


results =
  server_paths
  |> Enum.map(fn path ->
    IO.puts("Processing:  #{path}")

    opts[:verbose] && IO.puts("* Launching server:  #{path}")
    # Not bringing in a dependency just for this, so using a shell script
    #port = Port.open({:spawn, "bash tools/_stats.sh #{path}"}, [:binary, :stderr_to_stdout ])
    port = Helpers.server_run(path, opts)
    send(port, {self(), {:command, "ping\n"}})
    Process.sleep(2000)
    receive do
      {^port, {:data, "Launched\n"}} -> :ok
    after 5000 ->
      IO.puts("* * * ERROR: FAILED TO LAUNCH `#{path}` WITHIN 5s")
      Helpers.flush("Errors: ")
    end
    receive do
      {^port, {:data, "StoppedRunning\n"}} ->
        Port.close(port)
        IO.puts("Failed to launch server:\n```")
        Helpers.flush("")
        IO.puts("```\n")
        {path, %{}}
    after 2000 ->
      Helpers.flush(if(opts[:verbose], do: "Verbose: ", else: nil))

      opts[:verbose] && IO.puts("* * Warming up for #{opts.warmup}s #{length(urls)} times...")
      Enum.map(urls, fn {urlfn, script} ->
        id = :rand.uniform(100000000000000000000000000000)
        url = urlfn.(id)
        script = case script do nil -> []; s -> ["-s", "tools/wrk/#{s}.lua"] end
        opts[:verbose] && IO.puts("* * * Warming up URL:  #{url}")
        System.cmd("wrk", ["-t#{opts.threads}", "-c#{opts.connections}", "-d#{opts.warmup}s", "--timeout", "5", url | script])
      end)

      results =
        Enum.map(urls, fn {urlfn, script} ->
          id = :rand.uniform(100000000000000000000000000000)
          url = urlfn.(id)
          script = case script do nil -> []; s -> ["-s", "../#{s}.lua"] end
          opts[:verbose] && IO.puts("* URL:  #{url}")
          opts[:verbose] && IO.puts("* * Performing test for #{opts.duration}s...")
          {result, error_code} =
            System.cmd("wrk", [
              "-t#{opts.threads}", "-c#{opts.connections}", "-d#{opts.duration}s", "--timeout", "5", "--latency", url
              | script
            ])

          if error_code != 0 do
            IO.puts("* * * Test returned an unsuccessful error code, output of test of #{path}:\n#{result}\n")
          end

          opts[:verbose] && IO.puts("* * Test Complete")

          {urlfn.(0), Helpers.parse_stat_format(result)}
        end)

      try do
        opts[:verbose] && IO.puts("* Shutting down server:  #{path}\n")
        Port.close(port)
        Process.sleep(2000)
        Helpers.flush(if(opts[:verbose], do: "Verbose: ", else: nil))
      rescue ArgumentError ->
        IO.puts("* * * ERROR: Server shut down prior to end of test: #{path}")
        Helpers.flush("Errors: ")
        System.halt(1)
      end

      {path, Enum.into(results, %{})}
    end
  end)
  |> Enum.into(%{})


IO.puts("\n")

IO.puts("| Path | URL | Errors | Total Requests Count | Total Requests/s | Total Requests Throughput | Total Throughput/s | Req/s Avg | Req/s Stdev | Req/s Max | Req/s +/- | Latency Avg | Latency Stdev | Latency Max | Latency +/- | 50% | 75% | 90% | 99% |")
IO.puts("| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |")
Enum.map(results, fn {path, results} ->
  Enum.map(results, fn
    {_url, :error} -> []
    {url, r} ->
      IO.puts("| "<>Enum.join([
        path, url,
        r.errors,
        r.requests.requests, r.tot_req_sec, r.requests.read, r.tot_transfer_sec,
        r.req_sec.avg, r.req_sec.stdev, r.req_sec.max, r.req_sec.pmstdev,
        r.latency.avg, r.latency.stdev, r.latency.max, r.latency.pmstdev,
        r.perc50, r.perc75, r.perc90, r.perc99,
      ], " | ")<>" |")
  end)
end)

IO.puts("\n\n# Rankings\n")

IO.puts("## Ranking by Average Requests per second:")
results
|> Enum.map(fn {path, results} ->
  results
  |> Enum.filter(&(elem(&1, 1)!=:error))
  |> Enum.reduce({0.0, 0}, fn {_url, r}, {acc, c} ->
    {acc + r.tot_req_sec, c+1}
  end)
  |> case do
    {_, 0} -> {path, -1}
    {acc, c} -> {path, trunc(acc / c)}
  end
end)
|> Enum.sort_by(&-elem(&1, 1))
|> Enum.with_index(1)
|> Enum.map(fn {{path, avg}, idx} ->
  IO.puts("#{idx}. #{avg} req/sec : #{path}")
end)


System.stop(0)
end
