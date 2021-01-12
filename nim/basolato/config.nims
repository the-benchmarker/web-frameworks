
import os
# Logging
putEnv("LOG_IS_DISPLAY", "true")
putEnv("LOG_IS_FILE", "true")
putEnv("LOG_IS_ERROR", "false")
putEnv("LOG_DIR", "/root/project/examples/example/logs")
# Security
putEnv("SECRET_KEY", "QPyp/t^KTtw;xrN/Hzl&/AIr") # 24 length
putEnv("CSRF_TIME", "525600") # minutes of 1 year
putEnv("SESSION_TIME", "20160") # minutes of 2 weeks
putEnv("SESSION_DB", "/root/project/examples/example/session.db")
putEnv("IS_SESSION_MEMORY", "false")

putEnv("port", "3000")
