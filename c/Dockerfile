FROM debian:11.5

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential wget

{{#build_deps}}
  RUN apt-get -qy install {{{.}}}
{{/build_deps}}

WORKDIR /usr/src/app

{{#sources}}
   COPY {{{.}}} {{{.}}}
{{/sources}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
   RUN {{{.}}}
{{/build}}

FROM debian:11.5

WORKDIR /opt/web

RUN apt-get -qq update

{{#bin_deps}}
   RUN apt-get -qy install {{{.}}}
{{/bin_deps}}

{{#files}}
   COPY {{{.}}} {{{.}}}
{{/files}}

{{#binaries}}
   COPY --from=0 /usr/src/app/{{.}} {{{.}}}
{{/binaries}}

CMD {{{command}}}
