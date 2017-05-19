all: elixir node ruby crystal go rust swift python csharp client benchmarker

# --- Elixir ---
elixir: plug phoenix

plug:
	cd elixir/plug; echo "yes" | mix deps.get --force; MIX_ENV=prod mix release --no-tar
	ln -s -f ../elixir/plug/bin/server_elixir_plug bin/.

phoenix:
	cd elixir/phoenix; echo "yes" | mix do local.rebar; mix deps.get --force; MIX_ENV=prod mix release  --no-tar
	ln -s -f ../elixir/phoenix/bin/server_elixir_phoenix bin/.

# --- node.js ---
node: express clusterexpress

express:
	cd node/express; npm install
	ln -s -f ../node/express/bin/server_node_express bin/.
clusterexpress:
	cd node/express; npm install
	ln -s -f ../node/express/bin/server_node_clusterexpress bin/.

# --- Ruby ---
ruby: rails sinatra roda

# Rails
rails:
	cd ruby/rails; bundle update
	cd ruby/rails; bundle install --path vendor/bundle
	ln -s -f ../ruby/rails/bin/server_ruby_rails bin/.

# Sinatra
sinatra:
	cd ruby/sinatra; bundle update
	cd ruby/sinatra; bundle install --path vendor/bundle
	ln -s -f ../ruby/sinatra/server_ruby_sinatra bin/.

# Roda
roda:
	cd ruby/roda; bundle update
	cd ruby/roda; bundle install --path vendor/bundle
	ln -s -f ../ruby/roda/server_ruby_roda bin/.

# --- Crystal ---
crystal: kemal router_cr

# Kemal
kemal:
	cd crystal/kemal; shards build --release
	ln -s -f ../crystal/kemal/bin/server_crystal_kemal bin/.

# router.cr
router_cr:
	cd crystal/router.cr; shards build --release
	ln -s -f ../crystal/router.cr/bin/server_crystal_router_cr bin/.

# --- Go ---
go: echo gorilla-mux fasthttprouter

# Echo
echo:
	go get -u github.com/labstack/echo
	cd go/echo; go build -o server_go_echo main.go
	ln -s -f ../go/echo/server_go_echo bin/.

# gorilla/mux
gorilla-mux:
	go get -u github.com/gorilla/mux
	cd go/gorilla-mux; go build -o server_go_gorilla_mux main.go
	ln -s -f ../go/gorilla-mux/server_go_gorilla_mux bin/.

# fasthttprouter
fasthttprouter:
	go get -u github.com/buaazp/fasthttprouter
	go get -u github.com/valyala/fasthttp
	cd go/fasthttprouter; go build -o server_go_fasthttprouter main.go
	ln -s -f ../go/fasthttprouter/server_go_fasthttprouter bin/.

# --- Rust ---
rust: iron nickel rocket

# IRON
iron:
	cd rust/iron; cargo update
	cd rust/iron; cargo build --release
	ln -s -f ../rust/iron/target/release/server_rust_iron bin/.

# nickel.rs
nickel:
	cd rust/nickel; cargo update
	cd rust/nickel; cargo build --release
	ln -s -f ../rust/nickel/target/release/server_rust_nickel bin/.

# rocket
rocket:
	cd rust/rocket; cargo update
	cd rust/rocket; cargo build --release
	ln -s -f ../rust/rocket/target/release/server_rust_rocket bin/.

# --- Swift ---
swift: vapor perfect kitura

# Vapor
vapor:
	cd swift/vapor; swift build -c release
	ln -s -f ../swift/vapor/.build/release/server_swift_vapor bin/.

# Perfect
perfect:
	cd swift/perfect; swift build --configuration release
	ln -s -f ../swift/perfect/.build/release/server_swift_perfect bin/.

# Kitura
kitura:
	cd swift/kitura; swift build --configuration release
	ln -s -f ../swift/kitura/.build/release/server_swift_kitura bin/.

# --- C# ---
csharp: aspnetcore

# ASP.NET Core
aspnetcore:
	cd csharp/aspnetcore; dotnet restore && dotnet build
	ln -s -f ../csharp/aspnetcore/server_csharp_aspnetcore bin/.

# --- Python ---
python: sanic japronto

# Sanic
sanic:
	cd python/sanic; pip3 install -r requirements.txt; chmod +x server_python_sanic.py
	ln -s -f ../python/sanic/server_python_sanic.py bin/server_python_sanic

# Japronto 
japronto:
	cd python/japronto; pip3 install -r requirements.txt; chmod +x server_python_japronto.py
	ln -s -f ../python/japronto/server_python_japronto.py bin/server_python_japronto

# --- Benchmarker ---
# client
client:
	cd benchmarker; crystal build src/client.cr -o bin/client --release
	ln -s -f ../benchmarker/bin/client bin/.

# benchmarker
benchmarker:
	cd benchmarker; crystal build src/benchmarker.cr -o bin/benchmarker --release
	ln -s -f ../benchmarker/bin/benchmarker bin/.

# Cleaning all executables
clean:
	rm -rf bin/*
	rm -rf *.log
