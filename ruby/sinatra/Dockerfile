FROM ruby

WORKDIR /usr/src/app

COPY Gemfile app.rb config.ru ./

RUN bundle install

CMD bundle exec rackup -p 3000 -q -E production --host 0.0.0.0
