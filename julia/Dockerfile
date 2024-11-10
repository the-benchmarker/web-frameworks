{{#language.version}}
  FROM julia:{{{.}}}
{{/language.version}}
{{^language.version}}
  FROM julia:1.11
{{/language.version}}

WORKDIR /usr/src/app

COPY server.jl Project.toml deps.jl ./

RUN julia deps.jl

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK --interval=1m --timeout=60s --retries=3 CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{command}}
