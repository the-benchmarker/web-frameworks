ENV["LUCKY_ENV"] = "test"
require "spec"
require "../src/app"
require "./support/**"

Spec.after_each do
  LuckyRecord::Repo.truncate
end
