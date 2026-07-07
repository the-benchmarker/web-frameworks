package main

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"goyave.dev/goyave/v5"
	"goyave.dev/goyave/v5/util/errors"
)

// BenchmarkServer represents the benchmark HTTP server using Goyave framework
type BenchmarkServer struct {
	Server *goyave.Server
}

// newBenchmarkServer creates a new production-ready Goyave server
func newBenchmarkServer() (*BenchmarkServer, error) {
	// Production configuration
	server, err := goyave.New(goyave.Options{
		Host:         "",
		Port:         "3000",
		Environment:  goyave.EnvironmentProduction,
		Debug:        false,
		MaxBodySize:  16 * 1024 * 1024, // 16 MB
		ReadTimeout:  30 * time.Second,
		WriteTimeout: 30 * time.Second,
		IdleTimeout:  120 * time.Second,
	})
	if err != nil {
		return nil, err
	}

	return &BenchmarkServer{
		Server: server,
	}, nil
}

// addSecurityHeaders adds security headers to the Goyave response
func addSecurityHeaders(next goyave.Handler) goyave.Handler {
	return func(r *goyave.Response, req *goyave.Request) {
		r.Header.Set("X-Content-Type-Options", "nosniff")
		r.Header.Set("X-Frame-Options", "DENY")
		r.Header.Set("X-XSS-Protection", "1; mode=block")
		r.Header.Set("Content-Security-Policy", "default-src 'self'")
		r.Header.Set("Referrer-Policy", "strict-origin-when-cross-origin")
		r.Header.Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
		next(r, req)
	}
}

func empty(r *goyave.Response, _ *goyave.Request) {
	r.Header.Set("Content-Type", "text/plain")
	r.Status(http.StatusOK)
}

func healthCheck(r *goyave.Response, _ *goyave.Request) {
	r.Header.Set("Content-Type", "text/plain")
	r.Header.Set("Cache-Control", "no-cache, no-store, must-revalidate")
	r.Status(http.StatusOK)
	r.Write([]byte("OK"))
}

func getUser(r *goyave.Response, req *goyave.Request) {
	r.Header.Set("Content-Type", "text/plain")
	r.Status(http.StatusOK)
	r.Write([]byte(req.RouteParams["id"]))
}

func registerRoutes(_ *goyave.Server, router *goyave.Router) {
	router.Get("/", addSecurityHeaders(empty))
	router.Get("/health", addSecurityHeaders(healthCheck))
	router.Get("/user/{id}", addSecurityHeaders(getUser))
	router.Post("/user", addSecurityHeaders(empty))
}

func main() {
	// Create server instance
	server, err := newBenchmarkServer()
	if err != nil {
		fmt.Fprintln(os.Stderr, err.(*errors.Error).String())
		os.Exit(1)
	}

	// Register routes
	server.Server.RegisterRoutes(registerRoutes)

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port != "" {
		server.Server.Options.Port = port
	}

	// Start server in a goroutine for graceful shutdown
	go func() {
		log.Printf("Starting Goyave benchmark server on port %s", port)
		if err := server.Server.Start(); err != nil {
			server.Server.Logger.Error(err)
			os.Exit(2)
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

	// Shutdown the HTTP server
	if httpServer := server.Server.GetHttpServer(); httpServer != nil {
		if err := httpServer.Shutdown(ctx); err != nil {
			log.Printf("Server shutdown error: %v", err)
		}
	}

	log.Println("Server stopped")
}
