FROM dart:3.2 AS build

# Set the working directory
WORKDIR /app

# Add pubspec.yaml and get dependencies
COPY pubspec.yaml pubspec.yaml
RUN dart pub get --no-precompile

# Copy app source code and AOT compile it
ADD . .
RUN dart pub get --offline --no-precompile

# AOT compile `server.dart` to server executable
RUN dart compile exe server.dart -o server

# Build minimal serving image from AOT-compiled `server` executable
FROM scratch
COPY --from=build /runtime/ /
COPY --from=build /app/server /app/server

# Fix CI
COPY --from=build /bin/sh /bin/sh

CMD {{command}} {{options}}
