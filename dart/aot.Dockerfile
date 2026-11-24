FROM curlimages/curl:8.23.0 AS curl

FROM dart:3.14 AS build

# Set the working directory
WORKDIR /app

# Add pubspec.yaml and get dependencies
COPY pubspec.yaml pubspec.yaml
RUN dart pub get --no-precompile

# Copy app source code and refetch dependencies to cache
{{#files}}
    COPY '{{source}}' '{{target}}'
{{/files}}

RUN dart pub get --offline --no-precompile

# AOT compile `server.dart` into build/bundle/{bin,lib}. Unlike
# `dart compile exe`, `dart build cli` runs build hooks and bundles the
# native assets they produce into lib/.
RUN dart build cli -t server.dart -o build

# Build minimal serving image from the AOT-compiled bundle
FROM scratch

COPY --from=build /bin/sh /bin/sh
COPY --from=build /runtime/ /
COPY --from=build /app/build/bundle/ /app/

COPY --from=curl /usr/bin/curl curl
HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
