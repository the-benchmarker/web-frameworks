FROM debian:stable AS build

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install --no-install-recommends build-essential ldc dub openssl libssl-dev zlib1g-dev llvm-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub upgrade --verbose
RUN dub build -b release --compiler=ldc2

FROM debian:stable

WORKDIR /opt/web

RUN apt-get -qq update && apt-get -qy install --no-install-recommends openssl ldc \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/src/app/server /opt/web/server

USER nobody

CMD {{{command}}}
