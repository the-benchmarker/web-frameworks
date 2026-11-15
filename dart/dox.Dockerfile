FROM curlimages/curl:8.23.0 AS curl

FROM dart:3.14 AS build

WORKDIR /app

COPY pubspec.yaml pubspec.yaml
RUN dart pub get --no-precompile

{{#files}}
    COPY '{{source}}' '{{target}}'
{{/files}}

RUN dart pub get --offline --no-precompile

# `dart build cli` runs build hooks, so native assets (.so) are bundled.
# `dart compile exe` does not run them.
RUN dart build cli -t server.dart && cp -r build/cli/linux_*/bundle /bundle

# The native libraries need libgcc_s, which /runtime/ does not include.
RUN mkdir /extra && cp --parents /lib/*-linux-gnu/libgcc_s.so.1 /extra/

FROM scratch

COPY --from=build /bin/sh /bin/sh
COPY --from=build /runtime/ /
COPY --from=build /bundle/ /app/
COPY --from=build /extra/ /

COPY --from=curl /usr/bin/curl curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
