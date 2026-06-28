import os

# Production-grade configuration for Basolato framework
# Security and performance best practices applied

# Server configuration
putEnv("HOST", "0.0.0.0")
putEnv("PORT", "3000")

# Database configuration (disabled for production security)
# putEnv("DB_SQLITE", "false")
# putEnv("DB_POSTGRES", "false")
# putEnv("DB_MYSQL", "false")
# putEnv("DB_MARIADB", "false")

# Session configuration - using file-based sessions for simplicity
# For production, consider using Redis for distributed sessions
putEnv("SESSION_TYPE", "file")

# Disable features not needed in production
putEnv("LIBSASS", "false")

# Security headers and CORS settings
putEnv("ENABLE_CORS", "false")
putEnv("ALLOW_ORIGIN", "*")

# Debug and logging - DISABLED for production
putEnv("DEBUG", "false")
putEnv("LOG_LEVEL", "error")  # Only error logging in production
putEnv("VERBOSE", "false")

# Performance optimizations
putEnv("COMPRESSION", "true")
putEnv("CACHE_CONTROL", "max-age=3600")

# Security settings
putEnv("SECURE_COOKIES", "true")
putEnv("HTTP_ONLY_COOKIES", "true")
putEnv("SAME_SITE_COOKIES", "lax")
