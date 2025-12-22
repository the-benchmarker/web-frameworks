FROM debian:trixie-slim

WORKDIR /usr/src/app

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends build-essential wget && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

{{#build_deps}}
  RUN apt-get -qq update && apt-get -qy install --no-install-recommends {{{.}}} && apt-get clean && rm -rf /var/lib/apt/lists/*
{{/build_deps}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
   RUN {{{.}}}
{{/build}}

FROM debian:trixie-slim

WORKDIR /opt/web

{{#bin_deps}}
   RUN apt-get -qq update && apt-get -qy install --no-install-recommends {{{.}}} && apt-get clean && rm -rf /var/lib/apt/lists/*
{{/bin_deps}}

{{#binaries}}
   COPY --from=0 /usr/src/app/{{.}} {{{.}}}
{{/binaries}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
