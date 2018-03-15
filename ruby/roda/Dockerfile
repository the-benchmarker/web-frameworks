FROM ruby

WORKDIR /usr/src/app
COPY Gemfile  .
RUN bundle install
COPY . .

CMD bundle exec rackup -p 3000 -q -E production
