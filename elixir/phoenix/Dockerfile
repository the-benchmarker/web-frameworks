FROM elixir

RUN /usr/local/bin/mix local.hex --force && \
    /usr/local/bin/mix local.rebar --force && \
    /usr/local/bin/mix hex.info

WORKDIR /usr/src/app

COPY config config
COPY lib lib
COPY rel rel
COPY web web
COPY mix.exs ./

RUN echo "yes" | mix deps.get --force
ENV MIX_ENV prod
RUN mix release --no-tar

EXPOSE 3000
CMD _build/prod/rel/my_phoenix/bin/my_phoenix foreground
