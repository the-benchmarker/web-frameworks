package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/zeromicro/go-zero/core/logx"
	"github.com/zeromicro/go-zero/core/service"
	"github.com/zeromicro/go-zero/rest"
	"github.com/zeromicro/go-zero/rest/httpx"
)

// BenchmarkServer represents the benchmark HTTP server using Go-Zero framework
type BenchmarkServer struct {
	Engine *rest.Server
}

// newBenchmarkServer creates a new production-ready Go-Zero server
func newBenchmarkServer() *BenchmarkServer {
	// Production configuration
	engine := rest.MustNewServer(rest.RestConf{
		ServiceConf: service.ServiceConf{
			Log: logx.LogConf{
				Mode:       "console",
				Level:      "error",
				DisableStat: true,
			},
		},
		Port:         3000,
		Timeout:      30000,    // 30 seconds in milliseconds
		MaxConns:     1000000,
		MaxBytes:     16 * 1024 * 1024, // 16 MB
		CpuThreshold: 0,
	})

	return &BenchmarkServer{
		Engine: engine,
	}
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
	var v struct {
		ID string `path:"id"`
	}
	httpx.ParsePath(r, &v)
	w.Header().Set("Content-Type", "text/plain")
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(v.ID))
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

func main() {
	// Create server instance
	server := newBenchmarkServer()

	// Register routes with security headers
	server.Engine.AddRoutes([]rest.Route{
		{
			Method:  http.MethodGet,
			Path:    "/",
			Handler: addSecurityHeaders(http.HandlerFunc(rootHandler)),
		},
		{
			Method:  http.MethodGet,
			Path:    "/health",
			Handler: addSecurityHeaders(http.HandlerFunc(healthCheckHandler)),
		},
		{
			Method:  http.MethodGet,
			Path:    "/user/:id",
			Handler: addSecurityHeaders(http.HandlerFunc(getUserHandler)),
		},
		{
			Method:  http.MethodPost,
			Path:    "/user",
			Handler: addSecurityHeaders(http.HandlerFunc(createUserHandler)),
		},
	})

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port != "" {
		server.Engine.Config.Port = 0 // Will be overridden
	} else {
		port = "3000"
	}

	// Start server in a goroutine for graceful shutdown
	go func() {
		log.Printf("Starting Go-Zero benchmark server on port %s", port)
		if err := server.Engine.Start(); err != nil {
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

	if err := server.Engine.Stop(ctx); err != nil {
		log.Printf("Server shutdown error: %v", err)
	}

	log.Println("Server stopped")
}
