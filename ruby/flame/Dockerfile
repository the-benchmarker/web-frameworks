FROM ruby:2.6.1

WORKDIR /usr/src/app

COPY Gemfile application.rb config.ru ./
COPY controllers controllers

RUN bundle install

CMD bundle exec puma -p 3000 -e production -w $(nproc)
