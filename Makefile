all: ruby crystal go client

# --- Ruby ---
# 
# Rails
ruby: rails sinatra roda

rails:
	cd ruby/rails; bundle install --path vendor/bundle
	ln -s -f ../ruby/rails/bin/server_ruby_rails bin/.

sinatra:
	cd ruby/sinatra; bundle install --path vendor/bundle
	ln -s -f ../ruby/sinatra/server_ruby_sinatra bin/.

roda:
	cd ruby/roda; bundle install --path vendor/bundle
	ln -s -f ../ruby/roda/server_ruby_roda bin/.

# --- Crystal ---
# 
# Kemal route.cr
crystal: crystal-deps kemal route_cr

crystal-deps:
	cd crystal; shards update

# Kemal
kemal: crystal/src/server_kemal.cr
	cd crystal; crystal build src/server_kemal.cr -o bin/server_crystal_kemal --release
	ln -s -f ../crystal/bin/server_crystal_kemal bin/.

# route.cr
route_cr: crystal/src/server_route_cr.cr
	cd crystal; crystal build src/server_route_cr.cr -o bin/server_crystal_route_cr --release
	ln -s -f ../crystal/bin/server_crystal_route_cr bin/.

# --- Go ---
#
# Echo Gin
go: echo gorilla-mux

echo:
	go get -u github.com/labstack/echo
	cd go/echo; go build -o server_go_echo main.go
	ln -s -f ../go/echo/server_go_echo bin/.

gorilla-mux:
	go get -u github.com/gorilla/mux
	cd go/gorilla-mux; go build -o server_go_gorilla_mux main.go
	ln -s -f ../go/gorilla-mux/server_go_gorilla_mux bin/.

#
# Client -> bin/client
#
client: crystal/src/client.cr
	cd crystal; crystal build src/client.cr -o bin/client --release
	ln -s -f ../crystal/bin/client bin/.

# Cleaning all executables
clean:
	rm -rf bin/*
