package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/aofei/air"
)

// BenchmarkServer represents the benchmark HTTP server using Air framework
type BenchmarkServer struct {
	App    *air.Air
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready Air server
func newBenchmarkServer() *BenchmarkServer {
	a := air.New()
	a.Address = ":3000"
	a.DisableBanner = true
	a.MaxRequestBodySize = 16 * 1024 * 1024 // 16 MB

	return &BenchmarkServer{
		App: a,
	}
}

// addSecurityHeaders adds security headers to the Air response
func addSecurityHeaders(next air.Handler) air.Handler {
	return func(req *air.Request, res *air.Response) error {
		res.Header.Set("X-Content-Type-Options", "nosniff")
		res.Header.Set("X-Frame-Options", "DENY")
		res.Header.Set("X-XSS-Protection", "1; mode=block")
		res.Header.Set("Content-Security-Policy", "default-src 'self'")
		res.Header.Set("Referrer-Policy", "strict-origin-when-cross-origin")
		res.Header.Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
		return next(req, res)
	}
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func rootHandler(req *air.Request, res *air.Response) error {
	res.Header.Set("Content-Type", "text/plain")
	return res.WriteString("")
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func getUserHandler(req *air.Request, res *air.Response) error {
	res.Header.Set("Content-Type", "text/plain")
	return res.WriteString(req.Param("id").Value().String())
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func createUserHandler(req *air.Request, res *air.Response) error {
	res.Header.Set("Content-Type", "text/plain")
	return res.WriteString("")
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func healthCheckHandler(req *air.Request, res *air.Response) error {
	res.Header.Set("Content-Type", "text/plain")
	res.Header.Set("Cache-Control", "no-cache, no-store, must-revalidate")
	return res.WriteString("OK")
}

func main() {
	// Create server instance
	server := newBenchmarkServer()

	// Configure middleware
	server.App.Use(addSecurityHeaders)

	// Register routes
	server.App.GET("/", rootHandler)
	server.App.GET("/user/:id", getUserHandler)
	server.App.POST("/user", createUserHandler)
	server.App.GET("/health", healthCheckHandler)

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port != "" {
		server.App.Address = ":" + port
	} else {
		port = "3000"
	}

	// Create HTTP server with production-grade configuration
	server.Server = &http.Server{
		Addr:              server.App.Address,
		Handler:          server.App,
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Start server in a goroutine
	go func() {
		log.Printf("Starting Air benchmark server on port %s", port)
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
