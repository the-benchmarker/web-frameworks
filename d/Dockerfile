FROM debian:bookworm AS build

WORKDIR /usr/src/app

RUN apt-get -qq update
RUN apt-get -qy install build-essential openssl libssl-dev zlib1g-dev llvm-dev
RUN wget https://github.com/ldc-developers/ldc/releases/download/v1.36.0-beta1/ldc2-1.36.0-beta1-linux-x86_64.tar.xz
RUN rm -rf /usr/local/ldc2* && tar -C /usr/local -xvf ldc2-1.36.0-beta1-linux-x86_64.tar.xz
ENV PATH="$PATH:/usr/local/ldc2-1.36.0-beta1-linux-x86_64/bin"

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENV DC=ldc2
RUN dub build -b release --compiler=ldc2

FROM debian:bookworm

WORKDIR /opt/web

RUN apt-get -qq update && apt-get -qy install openssl ldc

COPY --from=build /usr/src/app/server /opt/web/server

USER nobody

CMD {{{command}}}
