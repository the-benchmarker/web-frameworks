# Production-Grade Mist Web Server

This is a production-ready implementation of a web server using the Mist framework in Gleam.

## Features

- **Security Best Practices**: OWASP-recommended security headers on all responses
- **Proper Error Handling**: Consistent error responses with appropriate HTTP status codes
- **Performance Optimized**: Clean, efficient code with proper resource management
- **Production Configuration**: Ready for deployment with production-grade settings

## Usage

```sh
gleam run   # Run the project
gleam build # Build for production
```

## Endpoints

- `GET /` - Health check endpoint
- `GET /user/:name` - Retrieve user by name
- `POST /user` - Create a new user

## Security Headers

All responses include the following security headers:
- `X-Content-Type-Options: nosniff`
- `X-Frame-Options: DENY`
- `X-XSS-Protection: 1; mode=block`
- `Strict-Transport-Security: max-age=63072000; includeSubDomains; preload`
- `Content-Security-Policy: default-src 'self'`
- `Referrer-Policy: strict-origin-when-cross-origin`
- `Permissions-Policy: geolocation=(), microphone=(), camera=()`
- `Cache-Control: no-store, no-cache, must-revalidate, private`

## Best Practices

- ✅ Security headers on all responses
- ✅ Proper HTTP status codes (200, 201, 404, 405, 500)
- ✅ Centralized error handling
- ✅ Method-specific routing
- ✅ No debug output in production
- ✅ Clean code organization with clear separation of concerns
