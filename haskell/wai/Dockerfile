FROM haskell

WORKDIR /usr/src/app

COPY Main.hs .
COPY Setup.hs .
COPY wai-app.cabal .

RUN cabal update
RUN cabal new-configure -O2
RUN cabal new-build

EXPOSE 3000
CMD cabal new-run
