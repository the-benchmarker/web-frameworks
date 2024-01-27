FROM debian:bookworm

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install --no-install-recommends build-essential git cmake \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

{{#build_deps}}
  RUN apt-get -qy install --no-install-recommends {{{.}}}
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

FROM debian:bookworm

{{#environment}}
ENV {{{.}}}
{{/environment}}

WORKDIR /opt/web

RUN apt-get -qq update

{{#bin_deps}}
   RUN apt-get -qy install --no-install-recommends {{{.}}}
{{/bin_deps}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}
{{#binaries}}
  COPY --from=0 /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

CMD {{{command}}}
