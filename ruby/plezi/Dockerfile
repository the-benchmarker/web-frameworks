FROM ruby:2.6

WORKDIR /usr/src/app

COPY Gemfile config.ru ./

RUN bundle install

CMD bundle exec iodine -w $(nproc) -p 3000 -t 1
