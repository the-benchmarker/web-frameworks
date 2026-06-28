package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"
)

// BenchmarkServer represents the benchmark HTTP server using standard net/http
type BenchmarkServer struct {
	Mux    *http.ServeMux
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready net/http server
func newBenchmarkServer() *BenchmarkServer {
	mux := http.NewServeMux()

	return &BenchmarkServer{
		Mux: mux,
	}
}

// addSecurityHeaders adds security headers to the response
func (s *BenchmarkServer) addSecurityHeaders(w http.ResponseWriter) {
	w.Header().Set("X-Content-Type-Options", "nosniff")
	w.Header().Set("X-Frame-Options", "DENY")
	w.Header().Set("X-XSS-Protection", "1; mode=block")
	w.Header().Set("Content-Security-Policy", "default-src 'self'")
	w.Header().Set("Referrer-Policy", "strict-origin-when-cross-origin")
	w.Header().Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
}

// registerRoutes registers all benchmark endpoints
func (s *BenchmarkServer) registerRoutes() {
	// Root endpoint
	s.Mux.HandleFunc("GET /", s.rootHandler)

	// User endpoints
	s.Mux.HandleFunc("GET /user/{id}", s.getUserHandler)
	s.Mux.HandleFunc("POST /user", s.createUserHandler)

	// Health check endpoint
	s.Mux.HandleFunc("GET /health", s.healthCheckHandler)
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func (s *BenchmarkServer) rootHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(""))
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func (s *BenchmarkServer) getUserHandler(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	w.Header().Set("Content-Type", "text/plain")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(id))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func (s *BenchmarkServer) createUserHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(""))
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func (s *BenchmarkServer) healthCheckHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.Header().Set("Cache-Control", "no-cache, no-store, must-revalidate")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("OK"))
}

func main() {
	// Create server instance
	server := newBenchmarkServer()

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
		Handler:          server.Mux,
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Start server in a goroutine
	go func() {
		log.Printf("Starting net/http benchmark server on port %s", port)
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
