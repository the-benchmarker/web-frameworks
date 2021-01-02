import os

# Security

putEnv("SECRET_KEY", "QPyp/t^KTtw;xrN/Hzl&/AIr")

# Logging

putEnv("LOG_IS_DISPLAY", "false")
putEnv("LOG_IS_FILE", "false")
putEnv("LOG_IS_ERROR_FILE", "false")
putEnv("LOG_DIR", "/opt/web/log")

# Session
putEnv("SESSION_TIME", "20160") # minutes of 2 weeks
putEnv("SESSION_DB_PATH", "/opt/web/session.db")
putEnv("SESSION_TYPE", "file")
putEnv("REDIS_PORT", "6379")
putEnv("COOKIE_DOMAINS", "")

putEnv("port", "3000")