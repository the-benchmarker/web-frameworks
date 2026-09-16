# frozen_string_literal: true

# Prints the -R (requests/second) for the fixed-rate latency pass that follows
# a closed-loop run. See helpers.rb#latency_rate_for: a percentage is taken
# of the framework's OWN closed-loop achieved_rate for that route and
# concurrency, so every framework is measured inside the load it can sustain
# and the number means "latency at N% of capacity"; a plain integer is an
# absolute rate, the same for everyone, and means "latency while serving N
# requests/second" (a framework that cannot sustain it shows rate_ratio < 1
# and a latency that grows with the backlog - which is the honest answer).
#
#   ruby .tasks/latency_rate.rb --closed <closed-run.json> --spec 50%
#   ruby .tasks/latency_rate.rb --closed <closed-run.json> --spec 20000

require 'json'
require 'optparse'
require_relative 'helpers'

options = {}
OptionParser.new do |opts|
  opts.on('--closed PATH', 'zrk JSON of the closed-loop run for this route') { |v| options[:closed] = v }
  opts.on('--spec SPEC', 'LATENCY_RATE: "N%" of the closed-loop rate, or an absolute N') { |v| options[:spec] = v }
end.parse!

raise '--spec is required' unless options[:spec]

achieved = nil
if options[:closed] && File.exist?(options[:closed])
  achieved = JSON.parse(File.read(options[:closed]))['achieved_rate']
end

rate = latency_rate_for(options[:spec], achieved)
if rate.nil?
  # A percentage with no closed-loop result to take it of: the closed run
  # failed, so this pass cannot be meaningful either. Keep the Makefile
  # moving with a rate zrk accepts and say why on stderr.
  warn "[latency_rate] #{options[:spec]} needs the closed-loop achieved_rate from #{options[:closed]}, " \
       'which is missing; falling back to 1000 req/s'
  rate = 1000
end
puts rate
