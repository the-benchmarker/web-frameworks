all: elixir node ruby crystal go rust swift python nim csharp scala java cpp

# --- Elixir ---
elixir: plug phoenix

plug:
	docker build -t plug elixir/plug

phoenix:
	docker build -t phoenix elixir/phoenix

# --- node.js ---
node: express fastify polka rayo

express:
	docker build -t express node/express

fastify:
	docker build -t fastify node/fastify

polka:
	docker build -t polka node/polka

rayo:
	docker build -t rayo node/rayo

# --- Objective-C ---
objc: criollo

#criollo
criollo:
	docker build -t criollo objc/criollo


# --- Ruby ---
ruby: rails sinatra roda rack-routing flame

# Rails
rails:
	docker build -t rails ruby/rails

# Sinatra
sinatra:
	docker build -t sinatra ruby/sinatra

# Roda
roda:
	docker build -t roda ruby/roda

# Rack Routing
rack-routing:
	docker build -t rack-routing ruby/rack-routing

# Flame
flame:
	docker build -t flame ruby/flame

# --- Crystal ---
crystal: kemal router.cr lucky amber raze spider-gazelle

# Kemal
kemal:
	docker build -t kemal crystal/kemal

# Raze
raze:
	docker build -t raze crystal/raze

# Lucky
lucky:
	docker build -t lucky crystal/lucky

# router.cr
router.cr:
	docker build -t router.cr crystal/router.cr

# amber
amber:
	docker build -t amber crystal/amber

# Spider Gazelle
spider-gazelle:
	docker build -t spider-gazelle crystal/spider-gazelle


# --- Go ---
go: echo gorilla-mux fasthttprouter gin iris

# Echo
echo:
	docker build -t echo go/echo

# gorilla/mux
gorilla-mux:
	docker build -t gorilla-mux go/gorilla-mux

iris:
	docker build -t iris go/iris

# fasthttprouter
fasthttprouter:
	docker build -t fasthttprouter go/fasthttprouter

gin:
	docker build -t gin go/gin

# --- Rust ---
rust: iron nickel rocket actix-web

# IRON
iron:
	docker build -t iron rust/iron

# nickel.rs
nickel:
	docker build -t nickel rust/nickel

# rocket
rocket:
	docker build -t rocket rust/rocket

# Actix
actix-web:
	docker build -t actix-web rust/actix-web

# --- Swift ---
swift: vapor perfect kitura

# Vapor
vapor:
	docker build -t vapor swift/vapor

# Perfect
perfect:
	docker build -t perfect swift/perfect

# Kitura
kitura:
	docker build -t kitura swift/kitura

# --- Scala ---
scala: akkahttp

# Akka-HTTP
akkahttp:
	docker build -t akkahttp scala/akkahttp

# --- C# ---
csharp: aspnetcore

# ASP.NET Core
aspnetcore:
	docker build -t aspnetcore csharp/aspnetcore

# --- Python ---
python: sanic japronto flask django tornado

# Sanic
sanic:
	docker build -t sanic python/sanic

# Japronto
japronto:
	docker build -t japronto python/japronto

# Flask
flask:
	docker build -t flask python/flask

# Django
django:
	docker build -t django python/django

# Tornado
tornado:
	docker build -t tornado python/tornado

# --- Nim ---
nim: jester mofuw

# Jester
jester:
	docker build -t jester nim/jester

# mofuw
mofuw:
	docker build -t mofuw nim/mofuw

# --- Java ---
java: act

# Act
act:
	docker build -t act java/act

# --- cpp ---
cpp: evhtp

evhtp:
	docker build -t evhtp cpp/evhtp

# --- php ---
php: symfony

symfony:
	docker build -t symfony php/symfony

# Cleaning all executables
clean:
	rm -rf bin/*
	rm -rf *.log
	find -type f -name '*.lock' -exec rm -fr {} \;
	find -type f -name package-lock.json -exec rm -fr {} \;
