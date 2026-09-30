FROM debian:stable-slim AS build

WORKDIR /opt/vlang

RUN apt-get -qq update && \
  apt-get -qy install --no-install-recommends \
    build-essential \
    git && \
  {{#build_deps.length}}
  apt-get -y install {{#build_deps}}{{.}} {{/build_deps}} && \
  {{/build_deps.length}}
  rm -rf /var/lib/apt/lists/*

ENV PATH=/opt/vlang:$PATH \
  GIT_SSL_NO_VERIFY=1


# `make` bootstraps V by cloning vlang/vc -- the C-translated compiler,
# regenerated from vlang/v master -- and pinning V's source without pinning vc
# leaves a time bomb: vc keeps moving, so eventually a newer bootstrap compiles
# this older source and a check it never had to satisfy fires. That is exactly
# what happened here, with 0.5.2's `fn C.open(&char, i32, i32) i32` clashing
# against os.c.v once vc caught up to vlang/v#27964 (fixed upstream after 0.5.2
# by making the last parameter variadic).
#
# So pin vc to the commit generated from V 0.5.2 itself and build with
# `local=1`, which stops the makefile pulling vc forward again. `local` also
# gates the tcc fetch, and the bootstrap links libgc out of that bundle -- so
# run `fresh_tcc` first, while it still applies.
RUN git clone https://github.com/vlang/v /opt/vlang && \
    cd /opt/vlang && \
    git checkout 7647ce1c6fad63b5578bc07883139906de74b2f8 && \
    make fresh_tcc && \
    git clone --filter=blob:none https://github.com/vlang/vc ./vc && \
    git -C ./vc checkout 7eb8c54a3843e5107d5af06d7a8c3e928f322475 && \
    make local=1

RUN ln -s /opt/vlang/v /usr/bin/v

{{#download}}
  RUN {{{.}}}
{{/download}}

WORKDIR /app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

{{#bootstrap}}
  RUN {{{.}}}
{{/bootstrap}}

RUN v {{#build_flags}} {{{.}}} {{/build_flags}} . -o server



FROM debian:stable-slim

WORKDIR /app

RUN apt-get -qq update && \
  apt-get -y install --no-install-recommends curl liburing2 && \
  rm -rf /var/lib/apt/lists/*

# Copy the built binary
COPY --from=build /app /app

HEALTHCHECK --interval=30s --timeout=10s --retries=3 --start-period=40s \
  CMD curl --fail http://0.0.0.0:3000 || exit 1

ENTRYPOINT {{{command}}}
