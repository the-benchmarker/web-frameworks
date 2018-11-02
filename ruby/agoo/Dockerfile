FROM ruby:2.5.3

WORKDIR /usr/src/app

COPY Gemfile app.rb ./

RUN bundle install

CMD nproc=256 ruby app.rb
