ARG LUTE_VERSION=1.0.0

FROM buildpack-deps:curl AS bin

RUN apt-get update && apt-get install -y --no-install-recommends unzip \
    && rm -rf /var/lib/apt/lists/*

ARG LUTE_VERSION
ARG TARGETARCH

RUN ARCH=$(echo "$TARGETARCH" | sed -e 's/arm64/aarch64/' -e 's/amd64/x86_64/') \
    && curl -fsSL "https://github.com/luau-lang/lute/releases/download/v${LUTE_VERSION}/lute-linux-${ARCH}.zip" --output lute.zip \
    && unzip lute.zip \
    && chmod +x lute



FROM gcr.io/distroless/cc-debian13

ARG LUTE_VERSION
ENV LUTE_VERSION=${LUTE_VERSION}

COPY --from=bin /lute /usr/local/bin/lute

WORKDIR /usr/src/app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

ENTRYPOINT {{{command}}}
