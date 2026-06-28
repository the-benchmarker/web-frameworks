package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/aerogo/aero"
)

// BenchmarkServer represents the benchmark HTTP server using Aero framework
type BenchmarkServer struct {
	App    *aero.Application
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready Aero server
func newBenchmarkServer() *BenchmarkServer {
	app := aero.New()
	app.Config.Ports.HTTP = 3000
	app.Config.DisableBanner = true
	app.Config.DisableErrorStack = true
	app.Config.MaxRequestBodySize = 16 * 1024 * 1024 // 16 MB

	return &BenchmarkServer{
		App: app,
	}
}

// addSecurityHeaders adds security headers to the Aero response
func addSecurityHeaders(ctx aero.Context) error {
	ctx.Response().Header.Set("X-Content-Type-Options", "nosniff")
	ctx.Response().Header.Set("X-Frame-Options", "DENY")
	ctx.Response().Header.Set("X-XSS-Protection", "1; mode=block")
	ctx.Response().Header.Set("Content-Security-Policy", "default-src 'self'")
	ctx.Response().Header.Set("Referrer-Policy", "strict-origin-when-cross-origin")
	ctx.Response().Header.Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
	return ctx.Next()
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func rootHandler(ctx aero.Context) error {
	ctx.Response().Header.Set("Content-Type", "text/plain")
	return ctx.String("")
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func getUserHandler(ctx aero.Context) error {
	ctx.Response().Header.Set("Content-Type", "text/plain")
	return ctx.String(ctx.Get("id"))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func createUserHandler(ctx aero.Context) error {
	ctx.Response().Header.Set("Content-Type", "text/plain")
	return ctx.String("")
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func healthCheckHandler(ctx aero.Context) error {
	ctx.Response().Header.Set("Content-Type", "text/plain")
	ctx.Response().Header.Set("Cache-Control", "no-cache, no-store, must-revalidate")
	return ctx.String("OK")
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
	if port != "" {
		server.App.Config.Ports.HTTP = 0 // Disable aero's built-in server
	} else {
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
		log.Printf("Starting Aero benchmark server on port %s", port)
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
