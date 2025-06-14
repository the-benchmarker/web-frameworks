{{#language.version}}
  FROM swift:{{{.}}}
{{/language.version}}

{{^language.version}}
  FROM swift:6.1
{{/language.version}}

RUN apt -y update
RUN apt -y install libssl-dev zlib1g-dev libcurl4-openssl-dev uuid-dev

WORKDIR /usr/src/app

COPY Package.swift ./
COPY Sources/ ./Sources/

{{#build_environment}}
ENV {{{.}}}
{{/build_environment}}

RUN swift build -c release -Xswiftc -enforce-exclusivity=unchecked

{{#language.version}}
  FROM swift:{{{.}}}-slim
{{/language.version}}

{{^language.version}}
  FROM swift:6.1-slim
{{/language.version}}

{{#environment}}
ENV {{{.}}}
{{/environment}}

COPY --from=0 /usr/src/app/.build/release/server /usr/src/app/.build/release/server

RUN apt-get -qq update
RUN apt-get -qy install curl

ENTRYPOINT {{command}} {{options}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
