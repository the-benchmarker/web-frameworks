package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/julienschmidt/httprouter"
)

// BenchmarkServer represents the benchmark HTTP server using HTTPRouter framework
type BenchmarkServer struct {
	Router *httprouter.Router
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready HTTPRouter server
func newBenchmarkServer() *BenchmarkServer {
	router := httprouter.New()

	return &BenchmarkServer{
		Router: router,
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
	s.Router.GET("/", s.rootHandler)

	// User endpoints
	s.Router.GET("/user/:id", s.getUserHandler)
	s.Router.POST("/user", s.createUserHandler)

	// Health check endpoint
	s.Router.GET("/health", s.healthCheckHandler)

	// 404 handler
	s.Router.NotFound = s.notFoundHandler
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func (s *BenchmarkServer) rootHandler(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
	w.Header().Set("Content-Type", "text/plain")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusOK)
	fmt.Fprint(w, "")
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func (s *BenchmarkServer) getUserHandler(w http.ResponseWriter, r *http.Request, ps httprouter.Params) {
	w.Header().Set("Content-Type", "text/plain")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusOK)
	fmt.Fprintf(w, "%s", ps.ByName("id"))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func (s *BenchmarkServer) createUserHandler(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
	w.Header().Set("Content-Type", "text/plain")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusOK)
	fmt.Fprint(w, "")
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func (s *BenchmarkServer) healthCheckHandler(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
	w.Header().Set("Content-Type", "text/plain")
	w.Header().Set("Cache-Control", "no-cache, no-store, must-revalidate")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusOK)
	fmt.Fprint(w, "OK")
}

// notFoundHandler handles 404 Not Found
// @Summary Not Found
// @Description 404 handler
// @Produce plain
// @Success 404 {string} string "Not Found"
func (s *BenchmarkServer) notFoundHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusNotFound)
	fmt.Fprint(w, "Not Found")
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
		Handler:          server.Router,
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Start server in a goroutine
	go func() {
		log.Printf("Starting HTTPRouter benchmark server on port %s", port)
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
