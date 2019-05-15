require "file_utils"

logger =
  Dexter::Logger.new(
    io: STDOUT,
    level: Logger::Severity::INFO
  )

Lucky.configure do |settings|
  settings.logger = logger
end

Avram::Repo.configure do |settings|
  settings.logger = logger
end
