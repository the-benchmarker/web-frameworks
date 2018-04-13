FROM ruby

WORKDIR /usr/src/app
COPY Gemfile  .
RUN bundle install
COPY . .

CMD ["rails", "s", "-b", "0.0.0.0", "-e", "production"]
