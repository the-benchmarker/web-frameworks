require "file_utils"

logger =
  Dexter::Logger.new(
    io: nil,
    level: Logger::Severity::ERROR
  )

Lucky.configure do |settings|
  settings.logger = logger
end
