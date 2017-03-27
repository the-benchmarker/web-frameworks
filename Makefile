
all:
	crystal build ./src/server_route_cr.cr -o ./bin/server_route_cr --release
	crystal build ./src/server_kemal.cr -o ./bin/server_kemal_exe --release
	crystal build ./src/client.cr -o ./bin/client --release
