all: elixir node ruby crystal go rust swift python nim csharp scala client benchmarker

# --- Elixir ---
elixir: plug phoenix

plug:
	cd elixir/plug; echo "yes" | mix deps.get --force; MIX_ENV=prod mix release --no-tar
	ln -s -f ../elixir/plug/bin/server_elixir_plug bin/.

phoenix:
	cd elixir/phoenix; echo "yes" | mix do local.rebar; mix deps.get --force; MIX_ENV=prod mix release  --no-tar
	ln -s -f ../elixir/phoenix/bin/server_elixir_phoenix bin/.

# --- node.js ---
node: express clusterexpress polka clusterpolka

express:
	cd node/express; npm install
	ln -s -f ../node/express/bin/server_node_express bin/.
clusterexpress:
	cd node/express; npm install
	ln -s -f ../node/express/bin/server_node_clusterexpress bin/.
polka:
	cd node/polka; npm install
	ln -s -f ../node/polka/bin/server_node_polka bin/.
clusterpolka:
	cd node/polka; npm install
	ln -s -f ../node/polka/bin/server_node_clusterpolka bin/.

# --- Objective-C ---
objc: criollo

#criollo
criollo:
	cd objc/criollo; pod install; xcodebuild -workspace server_objc_criollo.xcworkspace -scheme server_objc_criollo -derivedDataPath ./derivedData
	ln -s -f ../objc/criollo/derivedData/Build/Products/Debug/server_objc_criollo.app/Contents/MacOS/server_objc_criollo bin/.


# --- Ruby ---
ruby: rails sinatra roda rack-routing

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

# Rack Routing
rack-routing:
	cd ruby/rack-routing; bundle update
	cd ruby/rack-routing; bundle install --path vendor/bundle
	ln -s -f ../ruby/rack-routing/server_ruby_rack-routing bin/.

# --- Crystal ---
crystal: kemal router_cr lucky amber raze spider-gazelle

# Kemal
kemal:
	cd crystal/kemal; shards update; shards build --release
	ln -s -f ../crystal/kemal/bin/server_crystal_kemal bin/.

# Raze
raze:
	cd crystal/raze; shards update; shards build --release
	ln -s -f ../crystal/raze/bin/server_crystal_raze bin/.

# Lucky
lucky:
	mkdir -p crystal/lucky/public
	echo '{}' > crystal/lucky/public/manifest.json
	cd crystal/lucky; bin/setup; shards build --release
	ln -s -f ../crystal/lucky/bin/server_crystal_lucky bin/.

# router.cr
router_cr:
	cd crystal/router.cr; shards update; shards build --release
	ln -s -f ../crystal/router.cr/bin/server_crystal_router_cr bin/.

# amber
amber:
	cd crystal/amber; shards update; shards build --release
	ln -s -f ../crystal/amber/bin/server_crystal_amber bin/.

# Spider Gazelle
spider-gazelle:
	cd crystal/spider-gazelle; shards update; shards build --release
	ln -s -f ../crystal/spider-gazelle/bin/app bin/server_crystal_spider-gazelle


# --- Go ---
go: echo gorilla-mux fasthttprouter gin iris

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

iris:
	go get -u github.com/kataras/iris
	cd go/iris; go build -o server_go_iris main.go
	ln -s -f ../go/iris/server_go_iris bin/.

# fasthttprouter
fasthttprouter:
	go get -u github.com/buaazp/fasthttprouter
	go get -u github.com/valyala/fasthttp
	cd go/fasthttprouter; go build -o server_go_fasthttprouter main.go
	ln -s -f ../go/fasthttprouter/server_go_fasthttprouter bin/.

gin:
	go get github.com/gin-gonic/gin
	cd go/gin; go build -o server_go_gin main.go
	ln -s -f ../go/gin/server_go_gin bin/.

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

# Actix
actix:
	cd rust/actix; cargo update
	cd rust/actix; cargo build --release
	ln -s -f ../rust/actix/target/release/server_rust_actix bin/.

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

# --- Scala ---
scala: akkahttp

# Akka-HTTP
akkahttp:
	cd scala/akkahttp; sbt assembly
	ln -s -f ../scala/akkahttp/bin/server_scala_akkahttp bin/.

# --- C# ---
csharp: aspnetcore

# ASP.NET Core
aspnetcore:
	cd csharp/aspnetcore; dotnet restore && dotnet build
	ln -s -f ../csharp/aspnetcore/server_csharp_aspnetcore bin/.

# --- Python ---
python: sanic japronto flask django

# Sanic
sanic:
	cd python/sanic; pip3 install -r requirements.txt -U --user; chmod +x server_python_sanic.py
	ln -s -f ../python/sanic/server_python_sanic.py bin/server_python_sanic

# Japronto
japronto:
	cd python/japronto; pip3 install -r requirements.txt -U --user; chmod +x server_python_japronto.py
	ln -s -f ../python/japronto/server_python_japronto.py bin/server_python_japronto

# Flask
flask:
	cd python/flask; pip3 install -r requirements.txt -U --user; chmod +x server_python_flask
	ln -s -f ../python/flask/server_python_flask.py bin/.
	ln -s -f ../python/flask/server_python_flask bin/server_python_flask

# Django
django:
	cd python/django; pip3 install -r requirements.txt -U --user
	ln -s -f ../python/django/server_python_django bin/server_python_django

# Tornado
tornado:
	cd python/tornado; pip3 install -r requirements.txt -U --user; chmod +x server_python_tornado.py
	ln -s -f ../python/tornado/server_python_tornado.py bin/server_python_tornado

# --- Nim ---
nim: jester mofuw

# Jester
jester:
	cd nim/jester; nimble install -y; nim c -d:release server_nim_jester.nim
	ln -s -f ../nim/jester/server_nim_jester bin/server_nim_jester

# mofuw
mofuw:
	cd nim/mofuw; nimble install -y; nim c -d:release server_nim_mofuw.nim
	ln -s -f ../nim/mofuw/server_nim_mofuw bin/server_nim_mofuw

# --- Benchmarker ---
# client
client:
	cd tools; crystal build src/client.cr -o bin/client --release
	ln -s -f ../tools/bin/client bin/.

# benchmarker
benchmarker:
	cd tools; crystal build src/benchmarker.cr -o bin/benchmarker --release
	ln -s -f ../tools/bin/benchmarker bin/.

# Cleaning all executables
clean:
	rm -rf bin/*
	rm -rf *.log
