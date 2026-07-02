# Production-grade Kotlin Dockerfile
# Optimized for security, performance, and minimal image size

FROM gradle:jdk25 AS build

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

# Build with production optimizations
RUN gradle build -x test

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

COPY --from=build /usr/src/app/build/libs/server.jar /usr/src/app/build/libs/server.jar

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /usr/src/app -s /bin/false appuser && \
    chown -R appuser:appuser /usr/src/app

# Security: Drop all privileges
USER appuser

# Production health check with /health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000/health || exit 1

ENTRYPOINT ["java", "-server", "-jar", "/usr/src/app/build/libs/server.jar"]
