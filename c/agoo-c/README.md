# Agoo C - Production-Grade Web Server

This is a production-optimized implementation of the Agoo C web framework with security best practices, disabled debug logging, and enhanced readability.

## Features

- **Security Hardened**: Disabled debug logging, added input validation, request size limits
- **Production Optimized**: Maximum performance with security flags, PIE/ASLR support
- **Readable Code**: Well-documented, consistent formatting, comprehensive error handling
- **Best Practices**: Security headers, proper resource cleanup, validated inputs

## Security Features

### Application-Level Security
- **Input Validation**: All user inputs are validated for length and format
- **Request Size Limits**: Maximum request size of 10MB to prevent DoS attacks
- **Error Handling**: Comprehensive error checking with proper error responses
- **Security Headers**: Standard security headers configured (when supported by framework)
- **Debug Disabled**: Debug logging disabled in production mode

### Build-Level Security
- **Stack Protection**: `-fstack-protector-strong` enabled
- **Fortify Source**: `-D_FORTIFY_SOURCE=2` for additional buffer overflow protection
- **Position Independent Executable**: ASLR support via PIE
- **Strip Debug Symbols**: Production builds have debug symbols removed

## Building

### Production Build (Recommended)
```bash
make production
```

This creates an optimized, stripped binary with all security features enabled.

### Development Build
```bash
make develop
```

This creates a debug build with symbols for debugging and development.

### Standard Build
```bash
make
```

Creates a production build (same as `make production`).

## Build Options

The Makefile includes the following optimization and security flags:

- **Optimization**: `-O3 -march=native -mtune=native`
- **Warnings**: `-Wall -Wextra -Wpedantic -Wshadow -Wconversion -Werror`
- **Security**: `-D_FORTIFY_SOURCE=2 -fstack-protector-strong -fPIE`
- **Linking**: `-pie -s` (Position Independent Executable + stripped symbols)

## Running

Start the server:
```bash
./simple
```

The server will listen on port 3000 by default.

## Endpoints

- `GET /` - Returns 200 OK
- `GET /user/*` - Returns the user ID from the URL path
- `POST /user` - Returns 200 OK

## Configuration

### Server Configuration
- **Port**: 3000 (configure in `main()` function)
- **IO Thread Ratio**: 1.0 (optimized for multi-core systems)
- **Poll Wait**: 0.01 seconds (10ms)
- **Eval Threads**: 1 (sufficient for most use cases)
- **Max Request Size**: 10MB (configurable via `MAX_REQUEST_SIZE`)

### Log Level
Production builds use `AGOO_LOG_ERROR` to only log errors. For development, you can change this to:
- `AGOO_LOG_DEBUG` - All debug messages
- `AGOO_LOG_INFO` - Informational messages
- `AGOO_LOG_WARN` - Warnings only
- `AGOO_LOG_ERROR` - Errors only (production default)

## Security Best Practices Implemented

1. **Input Validation**: All user inputs validated before processing
2. **Buffer Overflow Prevention**: Length checks on all string operations
3. **Resource Limits**: Maximum request size enforced
4. **Error Handling**: Proper error codes and cleanup
5. **Secure Defaults**: Debug disabled, minimal information disclosure
6. **Build Security**: Hardened compilation flags and options

## Development Workflow

1. **Debug Build**: Use `make develop` for debugging
2. **Production Build**: Use `make production` for deployment
3. **Testing**: Use `make test` to run the server
4. **Cleanup**: Use `make clean` to remove build artifacts
5. **Rebuild**: Use `make rebuild` for clean rebuild

## Docker Usage

Build and run in Docker:
```bash
docker build -t agoo-c-app .
docker run -p 3000:3000 agoo-c-app
```

The Dockerfile includes additional security hardening:
- Non-root user execution
- Clean package caches
- Health checks configured

## Performance Tuning

Adjust the following parameters based on your hardware:
- `agoo_io_loop_ratio`: Ratio of IO threads to processors (1.0 recommended)
- `agoo_poll_wait`: Poll wait time in seconds (0.01 = 10ms)
- `agoo_server.thread_cnt`: Number of evaluation threads
- `MAX_REQUEST_SIZE`: Maximum allowed request size

## Contributing

When making changes, please:
1. Maintain the existing code style and formatting
2. Add comprehensive comments for complex logic
3. Include input validation for all user inputs
4. Handle errors appropriately
5. Test both development and production builds