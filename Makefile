
all: ruby crystal client

# --- Ruby ---
# 
# Rails
ruby: rails

rails:
	cd ruby/rails; bundle install --path vendor/bundle
	ln -s -f ../ruby/rails/bin/server_ruby_rails bin/.

# --- Crystal ---
# 
# Kemal route.cr
crystal: kemal route_cr

# Kemal
kemal: src/server_kemal.cr
	crystal build src/server_kemal.cr -o bin/server_crystal_kemal --release

# route.cr
route_cr: src/server_route_cr.cr
	crystal build src/server_route_cr.cr -o bin/server_crystal_route_cr --release


#
# Client -> bin/client
#
client: src/client.cr
	crystal build src/client.cr -o bin/client --release

# Cleaning all executables
clean:
	rm -rf bin/*
