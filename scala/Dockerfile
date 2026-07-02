FROM sbtscala/scala-sbt:eclipse-temurin-alpine-25.0.3_9_1.12.11_3.8.4 AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN sbt universal:packageZipTarball



# Runtime stage
FROM eclipse-temurin:25.0.3_9-jre

WORKDIR /usr/src/app

# Install minimal runtime dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

COPY --from=build /usr/src/app/target target

RUN tar -xvzf target/universal/server-0.1.0-SNAPSHOT.tgz

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /usr/src/app -s /bin/false appuser && \
    chown -R appuser:appuser /usr/src/app

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}

