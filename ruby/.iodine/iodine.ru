require_relative 'iodine'
require 'rack'

use IodineRequestBody
run Rack::Builder.parse_file('config.ru')
