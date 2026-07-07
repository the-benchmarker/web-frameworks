package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/flamego/flamego"
)

// BenchmarkServer represents the benchmark HTTP server using Flamego framework
type BenchmarkServer struct {
	Flamego *flamego.Flamego
	Server  *http.Server
}

// newBenchmarkServer creates a new production-ready Flamego server
func newBenchmarkServer() *BenchmarkServer {
	f := flamego.New()
	f.SetConfig(&flamego.Config{
		Debug:       false,
		PrintRoutes: false,
	})

	return &BenchmarkServer{
		Flamego: f,
	}
}

// addSecurityHeaders adds security headers to the Flamego response
func addSecurityHeaders(next flamego.Handler) flamego.Handler {
	return func(c flamego.Context) {
		c.Response().Header.Set("X-Content-Type-Options", "nosniff")
		c.Response().Header.Set("X-Frame-Options", "DENY")
		c.Response().Header.Set("X-XSS-Protection", "1; mode=block")
		c.Response().Header.Set("Content-Security-Policy", "default-src 'self'")
		c.Response().Header.Set("Referrer-Policy", "strict-origin-when-cross-origin")
		c.Response().Header.Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
		next(c)
	}
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func rootHandler(c flamego.Context) {
	c.Response().Header.Set("Content-Type", "text/plain")
	c.String(http.StatusOK, "")
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func getUserHandler(c flamego.Context) {
	c.Response().Header.Set("Content-Type", "text/plain")
	c.String(http.StatusOK, c.Param("id"))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func createUserHandler(c flamego.Context) {
	c.Response().Header.Set("Content-Type", "text/plain")
	c.String(http.StatusOK, "")
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func healthCheckHandler(c flamego.Context) {
	c.Response().Header.Set("Content-Type", "text/plain")
	c.Response().Header.Set("Cache-Control", "no-cache, no-store, must-revalidate")
	c.String(http.StatusOK, "OK")
}

func main() {
	// Create server instance
	server := newBenchmarkServer()

	// Configure middleware
	server.Flamego.Use(addSecurityHeaders)

	// Register routes
	server.Flamego.Get("/", rootHandler)
	server.Flamego.Get("/user/{id}", getUserHandler)
	server.Flamego.Post("/user", createUserHandler)
	server.Flamego.Get("/health", healthCheckHandler)

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port == "" {
		port = "3000"
	}

	// Create HTTP server with production-grade configuration
	server.Server = &http.Server{
		Addr:              ":" + port,
		Handler:          server.Flamego,
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Start server in a goroutine
	go func() {
		log.Printf("Starting Flamego benchmark server on port %s", port)
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
