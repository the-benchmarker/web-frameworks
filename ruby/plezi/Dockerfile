FROM ruby:2.6

WORKDIR /usr/src/app

COPY Gemfile config.ru ./

RUN bundle install

CMD iodine -w -2 -t 2
