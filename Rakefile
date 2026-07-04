# frozen_string_literal: true

require 'dotenv'
require_relative '.tasks/helpers'

Dotenv.load

Dir.glob('.tasks/*.rake').each { import _1 }
