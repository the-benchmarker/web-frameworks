FROM fpco/alpine-haskell-stack:gmp-ghc-8.8.3

WORKDIR /usr/src/app

COPY . ./

RUN stack build

FROM alpine

COPY --from=0 /usr/src/app/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/servant-proj-exe/servant-proj-exe server

RUN apk add gmp

CMD ./server
