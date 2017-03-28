
all: route_cr kemal client

route_cr: src/server_route_cr.cr
	crystal build src/server_route_cr.cr -o bin/server_route_cr --release

kemal: src/server_kemal.cr
	crystal build src/server_kemal.cr -o bin/server_kemal_exe --release

client: src/client.cr
	crystal build src/client.cr -o bin/client --release
