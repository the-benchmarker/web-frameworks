FROM debian:13-slim AS build

# Set a non-root user for added security
RUN useradd -m ziguser

# Install dependencies (update to latest secure versions)
RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends \
    wget xz-utils \
    ca-certificates && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Download the specified Zig binary from the official website
ARG ZIG_VERSION=0.15.2
RUN wget https://ziglang.org/download/$ZIG_VERSION/zig-{{arch}}-linux-$ZIG_VERSION.tar.xz && \
    tar -xvf zig-{{arch}}-linux-$ZIG_VERSION.tar.xz && \
    rm zig-{{arch}}-linux-$ZIG_VERSION.tar.xz && \
    mv zig-{{arch}}-linux-$ZIG_VERSION /usr/local/zig

# Add Zig to the PATH
ENV PATH="/usr/local/zig:$PATH"

WORKDIR /home/ziguser

{{#files}}
COPY '{{source}}' '{{target}}'
RUN chown ziguser {{target}}
{{/files}}
RUN chown -R ziguser src

# Switch to the non-root user
USER ziguser

RUN zig build -Doptimize=ReleaseFast

FROM debian:13-slim

RUN apt-get -qq update && \
    apt-get -qy install --no-install-recommends ca-certificates curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

COPY --from=build /home/ziguser/zig-out/bin/httpz /server

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit
