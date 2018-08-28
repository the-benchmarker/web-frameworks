FROM ruby

WORKDIR /usr/src/app

COPY Gemfile config.ru ./
COPY apps apps
COPY config config

ENV HANAMI_ENV production

RUN bundle install

EXPOSE 3000

CMD bundle exec puma -p 3000 -e production -w $(nproc)
