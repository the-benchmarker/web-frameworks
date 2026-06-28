package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/gogearbox/gearbox"
)

// BenchmarkServer represents the benchmark HTTP server using Gearbox framework
type BenchmarkServer struct {
	App    *gearbox.Engine
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready Gearbox server
func newBenchmarkServer() *BenchmarkServer {
	app := gearbox.New(&gearbox.Settings{
		Prefork:               false,
		DisableStartupMessage: true,
		MaxRequestBodySize:   16 * 1024 * 1024, // 16 MB
	})

	return &BenchmarkServer{
		App: app,
	}
}

// addSecurityHeaders adds security headers to the Gearbox response
func addSecurityHeaders(next gearbox.Handler) gearbox.Handler {
	return func(c gearbox.Context) {
		c.SetHeader("X-Content-Type-Options", "nosniff")
		c.SetHeader("X-Frame-Options", "DENY")
		c.SetHeader("X-XSS-Protection", "1; mode=block")
		c.SetHeader("Content-Security-Policy", "default-src 'self'")
		c.SetHeader("Referrer-Policy", "strict-origin-when-cross-origin")
		c.SetHeader("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
		next(c)
	}
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func rootHandler(c gearbox.Context) {
	c.SetHeader("Content-Type", "text/plain")
	c.SendString("")
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func getUserHandler(c gearbox.Context) {
	c.SetHeader("Content-Type", "text/plain")
	c.SendString(c.Param("id"))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func createUserHandler(c gearbox.Context) {
	c.SetHeader("Content-Type", "text/plain")
	c.SendString("")
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func healthCheckHandler(c gearbox.Context) {
	c.SetHeader("Content-Type", "text/plain")
	c.SetHeader("Cache-Control", "no-cache, no-store, must-revalidate")
	c.SendString("OK")
}

func main() {
	// Create server instance
	server := newBenchmarkServer()

	// Configure middleware
	server.App.Use(addSecurityHeaders)

	// Register routes
	server.App.Get("/", rootHandler)
	server.App.Get("/user/:id", getUserHandler)
	server.App.Post("/user", createUserHandler)
	server.App.Get("/health", healthCheckHandler)

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

	// Start server in a goroutine
	go func() {
		log.Printf("Starting Gearbox benchmark server on port %s", port)
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
