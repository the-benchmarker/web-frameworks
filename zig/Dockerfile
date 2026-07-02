FROM debian:13-slim AS build

# Set a non-root user for added security
RUN useradd -m ziguser

# Install dependencies (update to latest secure versions)
RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    wget \
    xz-utils \
    ca-certificates && \
  rm -rf /var/lib/apt/lists/*

# Download the specified Zig binary from the official website
ARG ZIG_VERSION=0.15.2

RUN wget https://ziglang.org/download/$ZIG_VERSION/zig-{{arch}}-linux-$ZIG_VERSION.tar.xz && \
  tar -xvf zig-{{arch}}-linux-$ZIG_VERSION.tar.xz && \
  mv zig-{{arch}}-linux-$ZIG_VERSION /usr/local/zig

# Add Zig to the PATH
ENV PATH="/usr/local/zig:$PATH"

WORKDIR /home/ziguser

{{#files}}
  COPY --chown=ziguser:ziguser '{{source}}' '{{target}}'
{{/files}}

RUN chown -R ziguser src

# Switch to the non-root user
USER ziguser

RUN zig build -Doptimize=ReleaseFast



FROM debian:13-slim

# Install minimal runtime dependencies
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends \
    ca-certificates \
    curl && \
  rm -rf /var/lib/apt/lists/* && \
  apt-get clean

COPY --from=build /home/ziguser/zig-out/bin/httpz /server

# Security: Create non-root user for production
RUN useradd -r -u 1000 -g 1000 -m -d / -s /bin/false appuser && \
    chown -R appuser:appuser /server

# Security: Drop all privileges
USER appuser

# Health check with production settings
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD curl --fail --silent --max-time 5 http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
