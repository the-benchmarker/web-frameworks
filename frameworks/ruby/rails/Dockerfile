FROM ruby

WORKDIR /usr/src/app

COPY Gemfile Rakefile config.ru ./
COPY app app
COPY config config
COPY public public
COPY bin bin

RUN bundle install

EXPOSE 3000

CMD bundle exec puma -p 3000 -e production -w $(nproc)
