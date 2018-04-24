#!/bin/sh

if [ -x "$1" ]; then
  true
else
  echo 'File does not exist'
  exit 1
fi

"$1" </dev/zero  & #>/dev/null 2>&1 &
pid=$!
sleep 3
echo 'Launched'
kill -0 $pid > /dev/null 2>&1
if [ ! $? ]; then
  echo 'StoppedRunning'
fi

while read line; do true; done

kill -INT $pid
kill $pid
kill -9 $pid
case "$1" in
  *server_ruby*)         kill -9 $(ps aux | grep -P 'puma.\d' | awk '{print $2}');; # killall -9 does not always work for puma...
  *server_node*)         killall -9 node;;
  *server_python_flask*) kill -9 $(ps aux | grep -P 'gunicorn' | awk '{print $2}');; # or python's flask with gunicorn
  *server_python*)       killall -9 python3;;
  *server_*_plug)        sh $(dirname $(dirname "$2"))/elixir/plug/_build/prod/rel/my_plug/bin/my_plug stop;;
  *server_*_phoenix)     sh $(dirname $(dirname "$2"))/elixir/phoenix/_build/prod/rel/my_phoenix/bin/my_phoenix stop;;
  *server_*_akkahttp)    killall -9 sbt;;
  *server_*_aspnetcore)  killall -9 dotnet;;
  *)                     true;;
esac

