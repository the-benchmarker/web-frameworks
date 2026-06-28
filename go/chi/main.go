package main

import (
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/go-chi/chi/v5/middleware"
)

// BenchmarkServer represents the benchmark HTTP server using Chi framework
type BenchmarkServer struct {
	Router *chi.Mux
}

func main() {
	// Create Chi router with production configuration
	router := chi.NewRouter()
	
	// Configure server
	server := &BenchmarkServer{
		Router: router,
	}

	// Configure middleware
	server.configureMiddleware()
	
	// Register routes
	server.registerRoutes()

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port == "" {
		port = "3000"
	}

	// Create HTTP server
	httpServer := &http.Server{
		Addr:              ":" + port,
		Handler:          router,
		ReadHeaderTimeout: 10 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      30 * time.Second,
		MaxHeaderBytes:   16 * 1024, // 16 KB
	}

	// Start server in a goroutine
	go func() {
		log.Printf("Starting Chi benchmark server on port %s", port)
		if err := httpServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server error: %v", err)
		}
	}()

	// Graceful shutdown
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	<-quit
	
	log.Println("Shutting down server...")
	
	// Give the server a grace period to finish active connections
	ctx, cancel := signal.NotifyContext(quit, syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	
	if err := httpServer.Shutdown(ctx); err != nil {
		log.Printf("Server shutdown error: %v", err)
	}
	
	log.Println("Server stopped")
}

// configureMiddleware configures Chi middleware
func (s *BenchmarkServer) configureMiddleware() {
	// Recovery middleware for panic handling
	s.Router.Use(middleware.Recoverer)

	// Request ID middleware for tracing
	s.Router.Use(middleware.RequestID)

	// Real IP middleware for proper client IP detection
	s.Router.Use(middleware.RealIP)

	// Logger middleware (skip health check for benchmarking)
	s.Router.Use(middleware.LoggerWithConfig(middleware.LoggerConfig{
		Log: func(msg string) {
			log.Println(msg)
		},
		Skip: func(r *http.Request) bool {
			return r.URL.Path == "/health"
		},
	}))

	// Timeout middleware
	s.Router.Use(middleware.Timeout(30 * time.Second))

	// Custom error handler
	s.Router.Use(func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			defer func() {
				if err := recover(); err != nil {
					log.Printf("PANIC: %v", err)
					w.WriteHeader(http.StatusInternalServerError)
					w.Write([]byte("Internal Server Error"))
				}
			}()
			next.ServeHTTP(w, r)
		})
	})
}

// registerRoutes registers all benchmark endpoints
func (s *BenchmarkServer) registerRoutes() {
	// Root endpoint
	s.Router.Get("/", s.rootHandler)

	// User endpoints
	s.Router.Get("/user/{id}", s.getUserHandler)
	s.Router.Post("/user", s.createUserHandler)

	// Health check endpoint
	s.Router.Get("/health", s.healthCheckHandler)

	// 404 handler
	s.Router.NotFound(s.notFoundHandler)
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func (s *BenchmarkServer) rootHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
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
	id := chi.URLParam(r, "id")
	w.Header().Set("Content-Type", "text/plain")
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
	w.WriteHeader(http.StatusNotFound)
	w.Write([]byte("Not Found"))
}
