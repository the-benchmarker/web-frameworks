FROM fpco/alpine-haskell-stack:gmp-ghc-8.8.3 AS build

WORKDIR /usr/src/app

COPY . ./

RUN stack build

# hadolint ignore=DL3006
FROM alpine

COPY --from=build /usr/src/app/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/server/server server

RUN apk add gmp --no-cache

CMD ./server
