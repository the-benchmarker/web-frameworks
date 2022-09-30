FROM dlang2/ldc-ubuntu:1.26.0 AS build

WORKDIR /usr/src/app

COPY . ./

RUN dub build -b release

FROM ubuntu:jammy

RUN apt-get -qq update
{{#deps}}
  RUN apt-get -qy install {{{.}}}
{{/deps}}

COPY --from=build /usr/src/app/server /usr/src/app/server

CMD /usr/src/app/server
