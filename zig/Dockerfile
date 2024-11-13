FROM chainguard/zig:latest AS build

WORKDIR /app

{{#files}}
COPY '{{source}}' '{{target}}'
{{/files}}

RUN zig build -Doptimize=ReleaseFast

FROM debian

RUN apt-get -qq update 
RUN apt-get -qy install ca-certificates curl

COPY --from=build /app/zig-out/bin/httpz /server

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit
