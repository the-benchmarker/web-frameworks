FROM ruby

WORKDIR /usr/src/app

COPY Gemfile Rakefile config.ru ./
COPY app app
COPY config config
COPY public public
COPY bin bin

RUN bundle install

CMD bundle exec puma -p 8004 -e production -w $(nproc)
