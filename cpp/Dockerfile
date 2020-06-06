# hadolint ignore=DL3006
FROM gcc AS build

RUN apt-get update && apt-get -y install cmake --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src/app

{{#build_deps}}
  RUN apk add {{{.}}}
{{/build_deps}}

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
  COPY --from=build /usr/src/app/{{{.}}} {{{.}}}
{{/binaries}}

CMD {{#command}}{{{.}}}{{/command}}
