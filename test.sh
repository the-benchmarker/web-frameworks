#!/usr/bin/env bash

frameworks=(
  "router.cr" "raze" "kemal" "lucky" "amber" "spider-gazelle" "rails" "sinatra" "roda" "rack-routing" "flame"
  "jester" "mofuw" "flask" "japronto" "sanic" "tornado" "iris" "echo" "gorilla-mux" "fasthttprouter" "gin" "vapor"
  "perfect" "kitura" "actix-web" "iron" "rocket" "nickel" "express" "fastify" "polka" "plug" "phoenix" "evhtp" 
  "akkahttp" "aspnetcore"
)


for fwk in ${frameworks[@]} ; do
  make $fwk
  CID=`docker run -td ${fwk}`
  sleep 30 # warmup
  IP=`docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ${CID}`
  CODE=`curl -vIL -X GET "http://${IP}:3000/" | grep HTTP | tail -n1 | awk '{print $2}' | tr -d ' '`
  [[ "${CODE}" == "200" ]] && echo "GET on / OK for ${fwk}" || (>&2 echo "GET on / KO for ${fwk}")
  CODE=`curl -vIL -X GET "http://${IP}:3000/user/1" | grep HTTP | tail -n1 | awk '{print $2}' | tr -d ' '`
  [[ "${CODE}" == "200" ]] && echo "GET on /user/0 OK for ${fwk}" || (>&2 echo "GET on /user/0 KO for ${fwk}")
  CODE=`curl -vIL -X POST "http://${IP}:3000/user" | grep HTTP | tail -n1 | awk '{print $2}' | tr -d ' '`
  [[ "${CODE}" == "200" ]] && echo "POST on /user OK for ${fwk}" || (>&2 echo -s "POST on /user KO for ${fwk}")
  docker stop $(docker ps -aq)
done
