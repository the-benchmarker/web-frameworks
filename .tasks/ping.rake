# frozen_string_literal: true

task :ping do
  require 'net/http'

  req = Net::HTTP::Get.new('/')

  begin
    Net::HTTP.start(ENV['HOST'], 3000) { |http| http.request(req) }
  rescue EOFError
    sleep 1 and retry
  end
end
