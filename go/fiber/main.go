package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/gofiber/fiber/v3"
	"github.com/gofiber/fiber/v3/middleware/cors"
	"github.com/gofiber/fiber/v3/middleware/logger"
	"github.com/gofiber/fiber/v3/middleware/recover"
)

// BenchmarkServer represents the benchmark HTTP server using Fiber framework
type BenchmarkServer struct {
	App    *fiber.App
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready Fiber server
func newBenchmarkServer() *BenchmarkServer {
	// Create Fiber application with production configuration
	app := fiber.New(fiber.Config{
		CaseSensitive:             true,
		StrictRouting:             true,
		DisableHeaderNormalizing: false,
		DisableStartupMessage:     true,
		EnablePrintRoutes:        false,
		BodyLimit:                16 * 1024 * 1024, // 16 MB
		ReadBufferSize:           4096,
		WriteBufferSize:          4096,
		JSONEncoder:             fiber.DefaultJSONEncoder,
		JSONDecoder:             fiber.DefaultJSONDecoder,
		EnableIPValidation:      true,
	})

	return &BenchmarkServer{
		App: app,
	}
}

func main() {
	// Create server instance
	server := newBenchmarkServer()

	// Configure middleware
	server.configureMiddleware()

	// Register routes
	server.registerRoutes()

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port == "" {
		port = "3000"
	}

	// Create HTTP server with production-grade configuration
	server.Server = &http.Server{
		Addr:              ":" + port,
		Handler:          server.App,
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Start server in a goroutine for graceful shutdown
	go func() {
		log.Printf("Starting Fiber benchmark server on port %s", port)
		if err := server.Server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server error: %v", err)
		}
	}()

	// Graceful shutdown
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	<-quit

	log.Println("Shutting down server...")

	// Give the server a grace period to finish active connections
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	if err := server.Server.Shutdown(ctx); err != nil {
		log.Printf("Server shutdown error: %v", err)
	}

	log.Println("Server stopped")
}

// configureMiddleware configures production-grade Fiber middleware
func (s *BenchmarkServer) configureMiddleware() {
	// Recovery middleware for panic handling
	s.App.Use(recover.New(recover.Config{
		EnableStackTrace: false, // Disable stack trace in production
		StackTraceHandler: func(c *fiber.Ctx, e interface{}) {
			log.Printf("PANIC: %v", e)
		},
	}))

	// Request ID middleware for tracing
	s.App.Use(func(c *fiber.Ctx) error {
		// Generate request ID
		requestID := c.Get("X-Request-ID")
		if requestID == nil {
			requestID = fiber.UUID()
			c.Set("X-Request-ID", requestID)
		}
		c.Response().Header.Set("X-Request-ID", requestID.(string))
		return c.Next()
	})

	// CORS middleware with production configuration
	s.App.Use(cors.New(cors.Config{
		Next:             nil,
		AllowOrigins:     "*",
		AllowMethods:     "GET,POST,HEAD,PUT,DELETE,PATCH,OPTIONS",
		AllowHeaders:     "Origin,Content-Type,Accept,Authorization,X-Request-ID",
		AllowCredentials: false,
		ExposeHeaders:    "Content-Type,Content-Length,X-Request-ID",
		MaxAge:           86400, // 24 hours
	}))

	// Security headers middleware
	s.App.Use(func(c *fiber.Ctx) error {
		c.Response().Header.Set("X-Content-Type-Options", "nosniff")
		c.Response().Header.Set("X-Frame-Options", "DENY")
		c.Response().Header.Set("X-XSS-Protection", "1; mode=block")
		c.Response().Header.Set("Content-Security-Policy", "default-src 'self'")
		c.Response().Header.Set("Referrer-Policy", "strict-origin-when-cross-origin")
		c.Response().Header.Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
		return c.Next()
	})

	// Logger middleware (skip health check for benchmarking performance)
	s.App.Use(logger.New(logger.Config{
		Format:     "${time} - ${ip} - ${status} - ${method} ${path} ${latency}\n",
		TimeFormat: "2006-01-02T15:04:05Z07:00",
		TimeInterval: 1,
		Filter: func(c *fiber.Ctx) bool {
			return c.Request().URI().Path() == "/health"
		},
		Output: os.Stdout,
	}))
}

// registerRoutes registers all benchmark endpoints
func (s *BenchmarkServer) registerRoutes() {
	// Root endpoint
	s.App.Get("/", s.rootHandler)

	// User endpoints
	s.App.Get("/user/:id", s.getUserHandler)
	s.App.Post("/user", s.createUserHandler)

	// Health check endpoint
	s.App.Get("/health", s.healthCheckHandler)

	// 404 handler
	s.App.Use(s.notFoundHandler)
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func (s *BenchmarkServer) rootHandler(c *fiber.Ctx) error {
	c.Set("Content-Type", "text/plain")
	c.Set("X-Content-Type-Options", "nosniff")
	c.Set("X-Frame-Options", "DENY")
	c.Set("X-XSS-Protection", "1; mode=block")
	return c.SendStatus(fiber.StatusOK)
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func (s *BenchmarkServer) getUserHandler(c *fiber.Ctx) error {
	c.Set("Content-Type", "text/plain")
	c.Set("X-Content-Type-Options", "nosniff")
	c.Set("X-Frame-Options", "DENY")
	c.Set("X-XSS-Protection", "1; mode=block")
	return c.SendString(c.Params("id"))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func (s *BenchmarkServer) createUserHandler(c *fiber.Ctx) error {
	c.Set("Content-Type", "text/plain")
	c.Set("X-Content-Type-Options", "nosniff")
	c.Set("X-Frame-Options", "DENY")
	c.Set("X-XSS-Protection", "1; mode=block")
	return c.SendStatus(fiber.StatusOK)
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func (s *BenchmarkServer) healthCheckHandler(c *fiber.Ctx) error {
	c.Set("Content-Type", "text/plain")
	c.Set("Cache-Control", "no-cache")
	c.Set("X-Content-Type-Options", "nosniff")
	c.Set("X-Frame-Options", "DENY")
	return c.SendString("OK")
}

// notFoundHandler handles 404 Not Found
// @Summary Not Found
// @Description 404 handler
// @Produce plain
// @Success 404 {string} string "Not Found"
func (s *BenchmarkServer) notFoundHandler(c *fiber.Ctx) error {
	c.Set("Content-Type", "text/plain")
	c.Set("X-Content-Type-Options", "nosniff")
	c.Set("X-Frame-Options", "DENY")
	return c.SendStatus(fiber.StatusNotFound)
}
