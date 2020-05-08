FROM swift:5.2

RUN apt -y update
RUN apt -y install libssl-dev zlib1g-dev libcurl4-openssl-dev uuid-dev

WORKDIR /usr/src/app

{{#sources}}
  COPY {{{.}}} {{{.}}}
{{/sources}}

RUN swift build -c release

FROM swift:5.2-slim
COPY --from=0 /usr/src/app/.build/release/server /usr/src/app/.build/release/server
CMD /usr/src/app/.build/release/server {{options}}
