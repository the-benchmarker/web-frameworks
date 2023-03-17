Log.setup :none

Lucky::Session.configure do |settings|
  settings.key = "_benchmark_session"
end

Lucky::ErrorHandler.configure do |settings|
  settings.show_debug_output = false
end

Lucky::Server.configure do |settings|
  settings.secret_key_base = "u4PWnhZfOFXdTOtoiSBF+6jn0zHbYS6/yumo3WXYNSw="
  settings.host = "0.0.0.0"
  settings.port = (ENV["PORT"]? || 3000).to_i
end
