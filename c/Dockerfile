# hadolint ignore=DL3006
FROM gcc AS build

RUN apt-get update

{{#build_deps}}
   RUN apt-get -y install {{{.}}} --no-install-recommends
{{/build_deps}}

RUN apt-get clean && rm -rf /var/lib/apt/lists/*

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

# hadolint ignore=DL3006
FROM alpine

WORKDIR /opt/web

{{#bin_deps}}
   RUN apk add {{{.}}}
{{/bin_deps}}

{{#files}}
   COPY {{{.}}} {{{.}}}
{{/files}}

{{#binaries}}
   COPY --from=build /usr/src/app/{{.}} {{{.}}}
{{/binaries}}

CMD {{#command}}{{{.}}}{{/command}}
