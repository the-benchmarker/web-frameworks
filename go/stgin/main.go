package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/AminMal/stgin"
)

// BenchmarkServer represents the benchmark HTTP server using stgin framework
type BenchmarkServer struct {
	Server *http.Server
	App    *stgin.Server
}

// newBenchmarkServer creates a new production-ready stgin server
func newBenchmarkServer() *BenchmarkServer {
	app := stgin.NewServer(":3000")

	return &BenchmarkServer{
		App: app,
	}
}

// addSecurityHeaders adds security headers to the stgin response
func addSecurityHeaders(status stgin.Status) stgin.Status {
	return stgin.WithHeaders(status, map[string]string{
		"X-Content-Type-Options":    "nosniff",
		"X-Frame-Options":          "DENY",
		"X-XSS-Protection":         "1; mode=block",
		"Content-Security-Policy":   "default-src 'self'",
		"Referrer-Policy":           "strict-origin-when-cross-origin",
		"Permissions-Policy":        "geolocation=(), microphone=(), camera=()",
	})
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func rootHandler(request stgin.RequestContext) stgin.Status {
	return addSecurityHeaders(stgin.WithHeaders(stgin.Ok(stgin.Empty()), map[string]string{
		"Content-Type": "text/plain",
	}))
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func getUserHandler(request stgin.RequestContext) stgin.Status {
	id := request.PathParams.MustGet("id")
	return addSecurityHeaders(stgin.WithHeaders(stgin.Ok(stgin.Text(id)), map[string]string{
		"Content-Type": "text/plain",
	}))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func createUserHandler(request stgin.RequestContext) stgin.Status {
	return addSecurityHeaders(stgin.WithHeaders(stgin.Ok(stgin.Empty()), map[string]string{
		"Content-Type": "text/plain",
	}))
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func healthCheckHandler(request stgin.RequestContext) stgin.Status {
	return addSecurityHeaders(stgin.WithHeaders(stgin.Ok(stgin.Text("OK")), map[string]string{
		"Content-Type":    "text/plain",
		"Cache-Control":  "no-cache, no-store, must-revalidate",
	}))
}

func main() {
	// Create server instance
	server := newBenchmarkServer()

	// Register routes
	server.App.AddRoutes(
		stgin.GET("/", rootHandler),
		stgin.GET("/user/:id", getUserHandler),
		stgin.POST("/user", createUserHandler),
		stgin.GET("/health", healthCheckHandler),
	)

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port == "" {
		port = "3000"
	}

	// Create HTTP server with production-grade configuration
	server.Server = &http.Server{
		Addr:              ":" + port,
		Handler:          server.App.Handler(),
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Start server in a goroutine
	go func() {
		log.Printf("Starting stgin benchmark server on port %s", port)
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
