{{#language.version}}
  FROM julia:{{{.}}}
{{/language.version}}
{{^language.version}}
  FROM julia:1.13
{{/language.version}}

WORKDIR /usr/src/app

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

COPY server.jl Project.toml deps.jl ./

RUN julia deps.jl

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
