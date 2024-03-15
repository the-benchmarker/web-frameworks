FROM swift:5.10

RUN apt -y update
RUN apt -y install libssl-dev zlib1g-dev libcurl4-openssl-dev uuid-dev

WORKDIR /usr/src/app

COPY Package.swift ./
COPY Sources/ ./Sources/

{{#build_environment}}
ENV {{{.}}}
{{/build_environment}}

RUN swift build -c release -Xswiftc -enforce-exclusivity=unchecked

FROM swift:5.10-slim
{{#environment}}
ENV {{{.}}}
{{/environment}}
COPY --from=0 /usr/src/app/.build/release/server /usr/src/app/.build/release/server
CMD {{command}} {{options}}
