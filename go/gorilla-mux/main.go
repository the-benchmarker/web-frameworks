package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/gorilla/mux"
)

// BenchmarkServer represents the benchmark HTTP server using Gorilla Mux framework
type BenchmarkServer struct {
	Router *mux.Router
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready Gorilla Mux server
func newBenchmarkServer() *BenchmarkServer {
	router := mux.NewRouter()

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
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      30 * time.Second,
		MaxHeaderBytes:   16 * 1024, // 16 KB
	}

	// Start server in a goroutine
	go func() {
		log.Printf("Starting Gorilla Mux benchmark server on port %s", port)
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

// registerRoutes registers all benchmark endpoints
func (s *BenchmarkServer) registerRoutes() {
	// Root endpoint
	s.Router.HandleFunc("/", s.rootHandler).Methods("GET")

	// User endpoints
	s.Router.HandleFunc("/user/{id}", s.getUserHandler).Methods("GET")
	s.Router.HandleFunc("/user", s.createUserHandler).Methods("POST")

	// Health check endpoint
	s.Router.HandleFunc("/health", s.healthCheckHandler).Methods("GET")

	// 404 handler
	s.Router.NotFoundHandler = s.notFoundHandler
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
	vars := mux.Vars(r)
	w.Header().Set("Content-Type", "text/plain")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(vars["id"]))
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

// notFoundHandler handles 404 Not Found
// @Summary Not Found
// @Description 404 handler
// @Produce plain
// @Success 404 {string} string "Not Found"
func (s *BenchmarkServer) notFoundHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	s.addSecurityHeaders(w)
	w.WriteHeader(http.StatusNotFound)
	w.Write([]byte("Not Found"))
}
