package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/uptrace/bunrouter"
)

// BenchmarkServer represents the benchmark HTTP server using Bunrouter framework
type BenchmarkServer struct {
	Router *bunrouter.Router
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready Bunrouter server
func newBenchmarkServer() *BenchmarkServer {
	r := bunrouter.New()

	return &BenchmarkServer{
		Router: r,
	}
}

// addSecurityHeaders adds security headers to the response
func addSecurityHeaders(next bunrouter.HandlerFunc) bunrouter.HandlerFunc {
	return func(w http.ResponseWriter, req bunrouter.Request) error {
		w.Header().Set("X-Content-Type-Options", "nosniff")
		w.Header().Set("X-Frame-Options", "DENY")
		w.Header().Set("X-XSS-Protection", "1; mode=block")
		w.Header().Set("Content-Security-Policy", "default-src 'self'")
		w.Header().Set("Referrer-Policy", "strict-origin-when-cross-origin")
		w.Header().Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
		return next(w, req)
	}
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func rootHandler(w http.ResponseWriter, req bunrouter.Request) error {
	w.Header().Set("Content-Type", "text/plain")
	w.Write([]byte(""))
	return nil
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func getUserHandler(w http.ResponseWriter, req bunrouter.Request) error {
	w.Header().Set("Content-Type", "text/plain")
	name := req.Param("name")
	w.Write([]byte(name))
	return nil
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func createUserHandler(w http.ResponseWriter, req bunrouter.Request) error {
	w.Header().Set("Content-Type", "text/plain")
	w.Write([]byte(""))
	return nil
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func healthCheckHandler(w http.ResponseWriter, req bunrouter.Request) error {
	w.Header().Set("Content-Type", "text/plain")
	w.Header().Set("Cache-Control", "no-cache, no-store, must-revalidate")
	w.Write([]byte("OK"))
	return nil
}

func main() {
	// Create server instance
	server := newBenchmarkServer()

	// Register routes with security headers
	server.Router.GET("/", addSecurityHeaders(rootHandler))
	server.Router.GET("/user/:id", addSecurityHeaders(getUserHandler))
	server.Router.POST("/user", addSecurityHeaders(createUserHandler))
	server.Router.GET("/health", addSecurityHeaders(healthCheckHandler))

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port == "" {
		port = "3000"
	}

	// Create HTTP server with production-grade configuration
	server.Server = &http.Server{
		Addr:              ":" + port,
		Handler:          server.Router,
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Start server in a goroutine
	go func() {
		log.Printf("Starting Bunrouter benchmark server on port %s", port)
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
