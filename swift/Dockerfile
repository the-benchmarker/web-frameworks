{{#language.version}}
  FROM swift:{{{.}}}
{{/language.version}}

{{^language.version}}
  FROM swift:6.2
{{/language.version}}

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends libssl-dev zlib1g-dev libcurl4-openssl-dev uuid-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

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
  FROM swift:6.2-slim
{{/language.version}}

{{#environment}}
ENV {{{.}}}
{{/environment}}

COPY --from=0 /usr/src/app/.build/release/server /usr/src/app/.build/release/server

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

ENTRYPOINT {{command}} {{options}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1
