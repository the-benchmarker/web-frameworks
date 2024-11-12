FROM debian AS build

ARG ZIG_VER=0.13.0

RUN apt-get -qq update 
RUN apt-get -qy install curl xz-utils \
    && apt-get clean 

RUN curl https://ziglang.org/download/${ZIG_VER}/zig-linux-$(uname -m)-${ZIG_VER}.tar.xz -o zig-linux.tar.xz && \
    tar xf zig-linux.tar.xz && \
    mv zig-linux-$(uname -m)-${ZIG_VER}/ /opt/zig

WORKDIR /app

{{#files}}
COPY '{{source}}' '{{target}}'
{{/files}}

RUN /opt/zig/zig build -Doptimize=ReleaseFast

FROM debian

RUN apt-get -qq update 
RUN apt-get -qy install ca-certificates curl

COPY --from=build /app/zig-out/bin/httpz /server

ENTRYPOINT {{command}}

HEALTHCHECK CMD curl --fail http://0.0.0.0:3000 || exit
