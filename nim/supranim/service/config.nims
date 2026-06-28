# Production-grade Supranim service configuration
# Security best practices, performance optimizations, and clean code

# Set project path for source files
switch("path", "$projectDir/../src")
from os import parentDir, `/`

# Get base path for project configuration
let basepath = projectDir().parentDir()

# Define base path and SSL support
switch "define", "supranimBasePath:" & basepath
switch "define", "ssl"

# Production optimizations
--opt:speed
--assertions:off