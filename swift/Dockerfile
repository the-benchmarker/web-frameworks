FROM swift:6.3 AS build

WORKDIR /usr/src/app

RUN apt-get -qq update && \
  apt-get -qy install libssl-dev zlib1g-dev libcurl4-openssl-dev uuid-dev {{#patches.length}}patch{{/patches.length}} && \
  rm -rf /var/lib/apt/lists/*

COPY Package.swift ./

{{#dependency_lockfile}}
COPY '{{.}}' ./Package.resolved
{{/dependency_lockfile}}

RUN swift package resolve

{{#patches}}
COPY '{{.}}' '/tmp/{{.}}'
RUN chmod -R u+w .build/checkouts && patch --batch --forward -p1 < '/tmp/{{.}}'
{{/patches}}

COPY Sources/ ./Sources/

{{#build_environment}}
  ENV {{{.}}}
{{/build_environment}}

RUN swift build -c release -Xswiftc -enforce-exclusivity=unchecked



FROM swift:6.3-slim

WORKDIR /app

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends curl && \
  rm -rf /var/lib/apt/lists/*

{{#environment}}
  ENV {{{.}}}
{{/environment}}

COPY --from=build /usr/src/app/.build/release/server /app/.build/release/server

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

{{#options}}
  CMD {{{options}}}
{{/options}}
