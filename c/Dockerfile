FROM debian:bullseye

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install --no-install-recommends build-essential wget

{{#build_deps}}
  RUN apt-get -qy install --no-install-recommends {{{.}}}
{{/build_deps}}

RUN apt-get clean && rm -rf /var/lib/apt/lists/*

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
   RUN {{{.}}}
{{/build}}

FROM debian:bullseye

WORKDIR /opt/web

RUN apt-get -qq update

{{#bin_deps}}
   RUN apt-get -qy install --no-install-recommends {{{.}}}
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

CMD {{{command}}}
