FROM dlang2/ldc-ubuntu AS build

WORKDIR /usr/src/app

COPY . ./

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2 --verbose

FROM ubuntu:jammy

RUN apt-get -qq update
{{#deps}}
  RUN apt-get -qy install {{{.}}}
{{/deps}}

COPY --from=build /usr/src/app/server /usr/src/app/server

CMD /usr/src/app/server

