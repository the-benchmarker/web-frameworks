import os
putEnv("HOST", "0.0.0.0")
# putEnv("DB_SQLITE", $true) # "true" or "false"
# putEnv("DB_POSTGRES", $true) # "true" or "false"
# putEnv("DB_MYSQL", $true) # "true" or "false"
# putEnv("DB_MARIADB", $true) # "true" or "false"
putEnv("SESSION_TYPE", "file") # "file" or "redis"
putEnv("LIBSASS", $false) # "true" or "false"
