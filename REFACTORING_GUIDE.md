# Refactoring Guide - Web Framework Benchmarks

## Overview

This guide provides language-specific best practices for refactoring all framework implementations in the-benchmarker project. Each framework should maintain its benchmarking functionality while adopting modern best practices.

## Table of Contents

1. [General Best Practices](#general-best-practices)
2. [Python Best Practices](#python-best-practices)
3. [Java Best Practices](#java-best-practices)
4. [Go Best Practices](#go-best-practices)
5. [JavaScript/TypeScript Best Practices](#javascripttypescript-best-practices)
6. [Ruby Best Practices](#ruby-best-practices)
7. [Rust Best Practices](#rust-best-practices)
8. [PHP Best Practices](#php-best-practices)
9. [C# Best Practices](#c-best-practices)
10. [Other Languages](#other-languages)

## General Best Practices

All framework implementations should follow these general principles:

### 1. **Maintain Benchmark Compatibility**
- Keep the same endpoint structure: `/`, `/user/{id}`, `/user` (POST)
- Return the same response formats (empty string for root and create, ID for user)
- Use plain text content type for benchmarking
- Do not add authentication or other middleware that would affect benchmark results

### 2. **Add Health Check Endpoint**
- Add `/health` endpoint returning "OK"
- Use for monitoring and health checks
- Exclude from benchmarking metrics if possible

### 3. **Error Handling**
- Add proper error handling with appropriate HTTP status codes
- Log errors appropriately (debug for expected, error for unexpected)
- Return plain text error responses for benchmarking consistency

### 4. **Logging**
- Add structured logging for debugging
- Use appropriate log levels (debug for request tracing, info for startup, error for failures)
- Configure logging based on environment (more verbose in development)

### 5. **Configuration**
- Use environment variables for configurable parameters (PORT, HOST, etc.)
- Provide sensible defaults for benchmarking
- Document configuration options

### 6. **Documentation**
- Add module-level documentation
- Add function/method-level documentation
- Document endpoints and their purpose

### 7. **Performance Considerations**
- Disable features not needed for benchmarking (debug pages, documentation UI, etc.)
- Use production-optimized settings
- Configure appropriate timeouts and limits
- Set reasonable request body size limits (16MB recommended)

## Python Best Practices

### Code Structure
```python
# Always include future annotations for type hints
from __future__ import annotations

# Use type hints for all functions and variables
from typing import Any, Optional, Union, cast

# Use proper imports (absolute where possible)
import logging
import os
import sys
```

### Application Setup
```python
# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("benchmark.<framework>")

# Create application with proper configuration
app = Framework()
app.config['MAX_CONTENT_LENGTH'] = 16 * 1024 * 1024  # 16 MB
app.config['JSONIFY_PRETTYPRINT_REGULAR'] = False
```

### Route Handlers
```python
@app.route("/", methods=["GET"])
def index() -> Response:
    """
    Root endpoint handler.
    
    Returns:
        Response: Empty response for benchmarking.
    """
    logger.debug("Root endpoint accessed")
    return Response(response="", status=200, mimetype="text/plain")

@app.route("/user/<int:id>", methods=["GET"])
def get_user(id: int) -> Response:
    """
    Retrieve user by ID.
    
    Args:
        id: User identifier.
    
    Returns:
        Response: User ID as plain text.
    """
    logger.debug(f"User endpoint accessed with ID: {id}")
    return Response(response=str(id), status=200, mimetype="text/plain")
```

### Error Handling
```python
@app.errorhandler(Exception)
def handle_exception(error: Exception) -> Response:
    """Global exception handler."""
    logger.error(f"Unhandled exception: {error}", exc_info=True)
    return Response(
        response="Internal Server Error",
        status=500,
        mimetype="text/plain",
    )

# Or for frameworks without built-in error handling
try:
    # route logic
    return response
except Exception as error:
    logger.error(f"Error: {error}", exc_info=True)
    return Response(status=500, body="Internal Server Error", content_type="text/plain")
```

### Main Block
```python
if __name__ == "__main__":
    import os
    
    host = os.getenv("HOST", "0.0.0.0")
    port = int(os.getenv("PORT", 3000))
    
    logger.info(f"Starting server on {host}:{port}")
    app.run(host=host, port=port, debug=False)
```

### Framework-Specific Notes

#### FastAPI
- Use `Annotated` for path parameters with validation
- Use `response_class` for explicit response types
- Disable docs with `docs_url=None, redoc_url=None`
- Use `Path` for path parameter validation

#### Flask
- Use `Response` objects for explicit responses
- Configure `app.config` properly
- Use `app.errorhandler` for global error handling

#### Django
- Use type hints in views
- Create separate URL configuration
- Use `HttpRequest` and `HttpResponse` types
- Add proper docstrings

#### aiohttp
- Use `web.Request` and `web.Response` types
- Add middleware for error handling
- Use `cast` for type safety
- Configure application with proper settings

## Java Best Practices

### Imports and Class Structure
```java
package benchmark.<framework>;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

/**
 * Benchmark Application for <Framework>.
 */
@Slf4j
@RestController
@RequestMapping("/")
@Tag(name = "Benchmark", description = "Benchmark endpoints")
public class BenchmarkController {
    // handlers
}
```

### Endpoint Handlers
```java
/**
 * Root endpoint handler.
 * 
 * @return Empty response for benchmarking
 */
@GetMapping(value = "/", produces = MediaType.TEXT_PLAIN_VALUE)
@Operation(summary = "Root endpoint", description = "Root endpoint for benchmarking")
public String root() {
    log.debug("Root endpoint accessed");
    return "";
}

/**
 * Get user by ID.
 * 
 * @param id User identifier
 * @return User ID as string
 */
@GetMapping(value = "/user/{id}", produces = MediaType.TEXT_PLAIN_VALUE)
@Operation(summary = "Get user by ID", description = "Retrieve user information by ID")
public String userId(@PathVariable Integer id) {
    log.debug("User endpoint accessed with ID: {}", id);
    return id.toString();
}
```

### Main Application
```java
@SpringBootApplication
@Slf4j
public class BenchmarkApplication {
    
    public static void main(String[] args) {
        SpringApplication.run(BenchmarkApplication.class, args);
    }
}
```

### Configuration
Add to `pom.xml`:
```xml
<!-- Lombok -->
<dependency>
    <groupId>org.projectlombok</groupId>
    <artifactId>lombok</artifactId>
    <version>1.18.30</version>
    <scope>provided</scope>
</dependency>

<!-- Spring Doc OpenAPI -->
<dependency>
    <groupId>org.springdoc</groupId>
    <artifactId>springdoc-openapi-starter-webmvc-ui</artifactId>
    <version>2.5.0</version>
</dependency>
```

## Go Best Practices

### Package and Imports
```go
package main

import (
    "log"
    "net/http"
    "os"

    "github.com/<framework>/<package>"
)

// BenchmarkServer represents the HTTP server
type BenchmarkServer struct {
    Router *<Framework>Router
    Port   string
}
```

### Main Function
```go
func main() {
    // Configure framework
    router := <framework>.New()
    
    server := &BenchmarkServer{
        Router: router,
        Port:   "3000",
    }
    
    // Configure middleware
    server.configureMiddleware()
    
    // Register routes
    server.registerRoutes()
    
    // Get port from environment
    port := os.Getenv("PORT")
    if port != "" {
        server.Port = port
    }
    
    // Start server
    log.Printf("Starting server on port %s", server.Port)
    if err := server.Router.Run(":" + server.Port); err != nil {
        log.Fatalf("Failed to start server: %v", err)
    }
}
```

### Middleware Configuration
```go
func (s *BenchmarkServer) configureMiddleware() {
    // Recovery middleware
    s.Router.Use(middleware.Recover())
    
    // Logger middleware
    s.Router.Use(middleware.LoggerWithConfig(middleware.LoggerConfig{
        Skipper: func(c *<Framework>.Context) bool {
            return c.Request().URL.Path == "/health"
        },
    }))
    
    // Body limit
    s.Router.Use(middleware.BodyLimit("16M"))
    
    // Custom error handler
    s.Router.HTTPErrorHandler = func(err error, c *<Framework>.Context) {
        log.Printf("Error: %v", err)
        c.String(http.StatusInternalServerError, "Internal Server Error")
    }
}
```

### Route Handlers
```go
// rootHandler handles GET /
func (s *BenchmarkServer) rootHandler(c *<Framework>.Context) error {
    c.Response().Header().Set("Content-Type", "text/plain")
    return c.String(http.StatusOK, "")
}

// getUserHandler handles GET /user/:id
func (s *BenchmarkServer) getUserHandler(c *<Framework>.Context) error {
    id := c.Param("id")
    c.Response().Header().Set("Content-Type", "text/plain")
    return c.String(http.StatusOK, id)
}

// createUserHandler handles POST /user
func (s *BenchmarkServer) createUserHandler(c *<Framework>.Context) error {
    c.Response().Header().Set("Content-Type", "text/plain")
    return c.String(http.StatusOK, "")
}

// healthCheckHandler handles GET /health
func (s *BenchmarkServer) healthCheckHandler(c *<Framework>.Context) error {
    c.Response().Header().Set("Content-Type", "text/plain")
    return c.String(http.StatusOK, "OK")
}
```

## JavaScript/TypeScript Best Practices

### Imports and Setup
```javascript
/**
 * <Framework> Benchmark Server
 */

import express from 'express';
import logger from 'morgan';
import 'express-async-errors';

const app = express();

// Configure Express
app.set('etag', false);
app.set('x-powered-by', false);
app.set('trust proxy', true);

// Body parsing
app.use(express.json({ limit: '16mb' }));
app.use(express.urlencoded({ extended: true, limit: '16mb' }));

// Logging
if (process.env.NODE_ENV === 'development') {
  app.use(logger('dev'));
} else {
  app.use(logger('combined'));
}
```

### Route Handlers
```javascript
/**
 * Root endpoint handler
 * @route GET /
 */
app.get('/', (req, res) => {
  res.status(200).set('Content-Type', 'text/plain').send('');
});

/**
 * Get user by ID
 * @route GET /user/:id
 */
app.get('/user/:id', (req, res) => {
  const { id } = req.params;
  res.status(200).set('Content-Type', 'text/plain').send(id);
});

/**
 * Create user
 * @route POST /user
 */
app.post('/user', (req, res) => {
  res.status(200).set('Content-Type', 'text/plain').send('');
});

/**
 * Health check
 * @route GET /health
 */
app.get('/health', (req, res) => {
  res.status(200).set('Content-Type', 'text/plain').send('OK');
});
```

### Error Handling
```javascript
// Error handling middleware (must be last)
app.use((err, req, res, next) => {
  console.error('Error:', err);
  res.status(err.status || 500);
  res.set('Content-Type', 'text/plain');
  
  if (process.env.NODE_ENV === 'production') {
    res.send('');
  } else {
    res.send(err.message || 'Internal Server Error');
  }
});

// 404 handler
app.use((req, res) => {
  res.status(404).set('Content-Type', 'text/plain').send('Not Found');
});

// Export for testing
export default app;
```

### TypeScript Version
```typescript
import express, { Request, Response, NextFunction } from 'express';

interface BenchmarkRequest extends Request {
  params: {
    id?: string;
  };
}

const app = express();

// Type-safe handlers
const rootHandler = (req: BenchmarkRequest, res: Response) => {
  res.status(200).set('Content-Type', 'text/plain').send('');
};

const getUserHandler = (req: BenchmarkRequest, res: Response) => {
  const { id } = req.params;
  res.status(200).set('Content-Type', 'text/plain').send(id || '');
};

// Error handler with types
const errorHandler = (
  err: Error,
  req: BenchmarkRequest,
  res: Response,
  next: NextFunction
) => {
  console.error('Error:', err);
  res.status(500).set('Content-Type', 'text/plain').send('');
};
```

## Ruby Best Practices

### Setup
```ruby
#!/usr/bin/env ruby

require 'bundler/setup'
require '<framework>'
require 'logger'

# Configure logging
logger = Logger.new(STDOUT)
logger.level = Logger::INFO
logger.formatter = proc { |severity, datetime, progname, msg|
  "#{datetime} - #{severity} - #{msg}\n"
}

# Create application
app = <Framework>::Application.new
```

### Routes
```ruby
# Root endpoint
app.get '/' do
  logger.debug("Root endpoint accessed")
  [200, { 'Content-Type' => 'text/plain' }, ['']]
end

# Get user by ID
app.get '/user/:id' do |id|
  logger.debug("User endpoint accessed with ID: #{id}")
  [200, { 'Content-Type' => 'text/plain' }, [id.to_s]]
end

# Create user
app.post '/user' do
  logger.debug("Create user endpoint accessed")
  [200, { 'Content-Type' => 'text/plain' }, ['']]
end

# Health check
app.get '/health' do
  [200, { 'Content-Type' => 'text/plain' }, ['OK']]
end
```

### Error Handling
```ruby
# Custom error handler
app.error do |e|
  logger.error("Error: #{e.message}")
  logger.error(e.backtrace.join("\n"))
  [500, { 'Content-Type' => 'text/plain' }, ['Internal Server Error']]
end
```

## Rust Best Practices

### Main File
```rust
//! <Framework> Benchmark Server
//!
//! A high-performance benchmark server using <Framework>.

use std::env;
use log::{debug, error, info};
use <framework>::{
    get, post, web, App, HttpResponse, HttpServer, Responder,
};

/// Root endpoint handler
#[get("/")]
async fn root() -> impl Responder {
    debug!("Root endpoint accessed");
    HttpResponse::Ok().content_type("text/plain").body("")
}

/// Get user by ID
#[get("/user/{id}")]
async fn get_user(id: web::Path<String>) -> impl Responder {
    debug!("User endpoint accessed with ID: {}", id);
    HttpResponse::Ok().content_type("text/plain").body(id.into_inner())
}

/// Create user
#[post("/user")]
async fn create_user() -> impl Responder {
    debug!("Create user endpoint accessed");
    HttpResponse::Ok().content_type("text/plain").body("")
}

/// Health check
#[get("/health")]
async fn health_check() -> impl Responder {
    HttpResponse::Ok().content_type("text/plain").body("OK")
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::Builder::from_default_env()
        .format_timestamp(None)
        .init();

    info!("Starting server");
    
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    
    HttpServer::new(|| {
        App::new()
            .service(root)
            .service(get_user)
            .service(create_user)
            .service(health_check)
    })
    .bind(("0.0.0.0", port.parse::<u16>().unwrap()))?
    .run()
    .await
}
```

## PHP Best Practices

### Setup
```php
<?php

// Enable error reporting for development
error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('log_errors', '1');

// Autoloading (if applicable)
require __DIR__ . '/vendor/autoload.php';

// Create application
$app = new <Framework>\Application();
```

### Routes
```php
// Root endpoint
$app->get('/', function ($request, $response) {
    $response->getBody()->write('');
    return $response->withHeader('Content-Type', 'text/plain');
});

// Get user by ID
$app->get('/user/{id}', function ($request, $response, $args) {
    $id = $args['id'];
    $response->getBody()->write($id);
    return $response->withHeader('Content-Type', 'text/plain');
});

// Create user
$app->post('/user', function ($request, $response) {
    $response->getBody()->write('');
    return $response->withHeader('Content-Type', 'text/plain');
});

// Health check
$app->get('/health', function ($request, $response) {
    $response->getBody()->write('OK');
    return $response->withHeader('Content-Type', 'text/plain');
});
```

### Error Handling
```php
// Custom error handler
$app->addErrorMiddleware(function ($request, $response, $exception) {
    error_log('Error: ' . $exception->getMessage());
    error_log($exception->getTraceAsString());
    
    $response->getBody()->write('Internal Server Error');
    return $response->withStatus(500)->withHeader('Content-Type', 'text/plain');
});

// Run application
$app->run();
```

## C# Best Practices

### Program.cs
```csharp
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

var builder = WebApplication.CreateBuilder(args);

// Configure logging
builder.Logging.ClearProviders();
builder.Logging.AddConsole();

// Configure services
builder.Services.AddControllers();

var app = builder.Build();

// Configure middleware
app.UseHttpsRedirection();
app.UseRouting();
app.UseAuthorization();

// Configure routes
app.MapGet("/", () => Results.Text("", "text/plain"));
app.MapGet("/user/{id}", (int id) => Results.Text(id.ToString(), "text/plain"));
app.MapPost("/user", () => Results.Text("", "text/plain"));
app.MapGet("/health", () => Results.Text("OK", "text/plain"));

// Error handling
app.UseExceptionHandler(errorApp =>
{
    errorApp.Run(async context =>
    {
        context.Response.StatusCode = 500;
        context.Response.ContentType = "text/plain";
        await context.Response.WriteAsync("Internal Server Error");
    });
});

app.Run();
```

### Controller Version
```csharp
[ApiController]
[Route("")]
public class BenchmarkController : ControllerBase
{
    private readonly ILogger<BenchmarkController> _logger;

    public BenchmarkController(ILogger<BenchmarkController> logger)
    {
        _logger = logger;
    }

    [HttpGet("")]
    [Produces("text/plain")]
    public IActionResult Get()
    {
        _logger.LogDebug("Root endpoint accessed");
        return Ok("");
    }

    [HttpGet("user/{id}")]
    [Produces("text/plain")]
    public IActionResult GetUser(int id)
    {
        _logger.LogDebug("User endpoint accessed with ID: {Id}", id);
        return Ok(id.ToString());
    }

    [HttpPost("user")]
    [Produces("text/plain")]
    public IActionResult CreateUser()
    {
        _logger.LogDebug("Create user endpoint accessed");
        return Ok("");
    }

    [HttpGet("health")]
    [Produces("text/plain")]
    public IActionResult HealthCheck()
    {
        return Ok("OK");
    }
}
```

## Other Languages

For other languages (Elixir, Clojure, Haskell, OCaml, Lua, Perl, R, Crystal, Nim, Zig, V, etc.):

1. **Follow the language's established idioms and patterns**
2. **Use the framework's recommended practices**
3. **Add logging and error handling**
4. **Maintain the same endpoint structure**
5. **Use plain text responses**
6. **Add health check endpoint**

## Testing the Refactoring

After refactoring each framework, test that:

1. The application starts without errors
2. All endpoints return the expected responses:
   - `GET /` → empty string, 200
   - `GET /user/123` → "123", 200
   - `POST /user` → empty string, 200
   - `GET /health` → "OK", 200
3. Error handling works (try malformed requests)
4. Logging outputs are visible and informative
5. The benchmark suite can still run successfully

## Completion Checklist

For each framework, verify:

- [ ] Code follows language-specific best practices
- [ ] Type hints/types added where appropriate
- [ ] Documentation added (module, class, function level)
- [ ] Logging configured and used
- [ ] Error handling implemented
- [ ] Health check endpoint added
- [ ] Environment configuration supported
- [ ] No breaking changes to benchmarking functionality
- [ ] No unnecessary features that would affect benchmarks
- [ ] Code compiles/runs without errors

## Summary

This refactoring effort aims to improve code quality, maintainability, and consistency across all benchmark implementations while preserving the benchmarking functionality. Each framework should be refactored to follow its language's best practices as outlined in this guide.
