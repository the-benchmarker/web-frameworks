FROM dart:3.5 AS build

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

# AOT compile `server.dart` to server executable
RUN dart compile exe server.dart -o server

# Build minimal serving image from AOT-compiled `server` executable
FROM scratch
COPY --from=build /bin/sh /bin/sh

COPY --from=build /runtime/ /
COPY --from=build /app/server /app/server

RUN apt-get -qq update
RUN apt-get -qy install curl
HEALTHCHECK --interval=1m --timeout=60s --retries=3 CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{command}}
