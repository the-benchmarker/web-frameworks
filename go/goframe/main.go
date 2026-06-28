package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"github.com/gogf/gf/v2/frame/g"
)

const (
	routeRoot   = `/`
	routeUser   = `/user`
	routeUserId = `/user/`
	routeHealth = `/health`
)

// BenchmarkServer represents the benchmark HTTP server using GoFrame framework
type BenchmarkServer struct {
	Server *g.Server
}

// addSecurityHeaders adds security headers to the response
func addSecurityHeaders(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("X-Content-Type-Options", "nosniff")
		w.Header().Set("X-Frame-Options", "DENY")
		w.Header().Set("X-XSS-Protection", "1; mode=block")
		w.Header().Set("Content-Security-Policy", "default-src 'self'")
		w.Header().Set("Referrer-Policy", "strict-origin-when-cross-origin")
		w.Header().Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
		next.ServeHTTP(w, r)
	})
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func rootHandler(w http.ResponseWriter, r *http.Request) {
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
func getUserHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	id := r.URL.Path[len(routeUserId):]
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(id))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func createUserHandler(w http.ResponseWriter, r *http.Request) {
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
func healthCheckHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.Header().Set("Cache-Control", "no-cache, no-store, must-revalidate")
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("OK"))
}

// notFoundHandler handles 404 Not Found
// @Summary Not Found
// @Description 404 handler
// @Produce plain
// @Success 404 {string} string "Not Found"
func notFoundHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	w.WriteHeader(http.StatusNotFound)
	w.Write([]byte("Not Found"))
}

func main() {
	// Create server instance
	s := g.Server()
	s.SetPort(3000)

	// Configure production settings
	s.SetConfigWithMap(g.Map{
		"server.Debug":           false,
		"server.LogPath":        "",
		"server.AccessLog":      false,
		"server.ErrorLog":       false,
		"server.MaxHeaderBytes": 1 << 20, // 1 MB
	})

	// Create HTTP server with production-grade configuration
	httpServer := &http.Server{
		Addr:              ":3000",
		Handler:          addSecurityHeaders(s.GetHttpServer()),
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Configure routes
	httpServer.Handler = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		addSecurityHeaders(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			switch r.URL.Path {
			case routeRoot:
				rootHandler(w, r)
			case routeUser:
				if r.Method == http.MethodPost {
					createUserHandler(w, r)
				}
			case routeHealth:
				healthCheckHandler(w, r)
			default:
				if strings.HasPrefix(r.URL.Path, routeUserId) {
					getUserHandler(w, r)
				} else {
					notFoundHandler(w, r)
				}
			}
		})(w, r)
	})

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port == "" {
		port = "3000"
	} else {
		httpServer.Addr = ":" + port
	}

	// Start server in a goroutine
	go func() {
		log.Printf("Starting GoFrame benchmark server on port %s", port)
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
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	if err := httpServer.Shutdown(ctx); err != nil {
		log.Printf("Server shutdown error: %v", err)
	}

	log.Println("Server stopped")
}
