import os

# DB Connection
# putEnv("DB_DRIVER", "postgres")
# putEnv("DB_CONNECTION", "localhost:5432")
# putEnv("DB_USER", "postgres")
# putEnv("DB_PASSWORD", "")
# putEnv("DB_DATABASE", "benchmark")

# Logging
putEnv("LOG_IS_DISPLAY", "false")
putEnv("LOG_IS_FILE", "false")
putEnv("LOG_DIR", "/usr/src/app/logs")

# Security
putEnv("SECRET_KEY", "2#~p~meIs9ncV.h9d{j@U|l*") # 24 length
putEnv("CSRF_TIME", "525600") # minutes of 1 year
putEnv("SESSION_TIME", "20160") # minutes of 2 weeks
putEnv("SESSION_DB", "/usr/src/app/session.db")
putEnv("IS_SESSION_MEMORY", "false")
