# Production-grade Java Dockerfile
# Optimized for security, performance, and minimal image size

{{#language.compiler.gradle}}
  FROM gradle:9-jdk25 AS build
{{/language.compiler.gradle}}
{{#language.compiler.maven}}
  {{#language.java}}
    FROM maven:3-eclipse-temurin-{{{.}}} AS build
  {{/language.java}}
  {{^language.java}}
    FROM maven:3-eclipse-temurin-25 AS build
  {{/language.java}}
{{/language.compiler.maven}}

WORKDIR /usr/src/web

{{#build_deps.length}}
  ARG DEBIAN_FRONTEND=noninteractive
  RUN apt-get -qq update && \
    apt-get -y install --no-install-recommends {{#build_deps}}{{{.}}} {{/build_deps}} && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get clean
{{/build_deps.length}}

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#patch}}
  {{{.}}}
{{/patch}}

{{#build_fix}}
  RUN {{{.}}}
{{/build_fix}}

{{#build}}
  RUN {{{.}}}
{{/build}}

{{^build}}
  RUN mvn package -DskipTests
{{/build}}

# Runtime stage with minimal JRE
{{#language.java}}
  FROM eclipse-temurin:{{{.}}}-jre
{{/language.java}}
{{^language.java}}
  FROM eclipse-temurin:25.0.3_9-jre
{{/language.java}}

# Install minimal runtime dependencies
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

WORKDIR /opt/web

{{#binaries}}
  COPY --from=build /usr/src/web/{{{.}}} {{{.}}}
{{/binaries}}

{{#static_files}}
  COPY '{{source}}' '{{target}}'
{{/static_files}}

RUN ln -sfv /opt/java/openjdk/bin/java /usr/bin/java

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

{{#environment}}
  ENV {{{.}}}
{{/environment}}

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d /opt/web -s /bin/false appuser && \
    chown -R appuser:appuser /opt/web

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
