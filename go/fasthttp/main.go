package main

import (
	"bytes"
	"context"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/valyala/fasthttp"
)

// BenchmarkServer represents the benchmark HTTP server using Fasthttp framework
type BenchmarkServer struct {
	Server *fasthttp.Server
}

// newBenchmarkServer creates a new production-ready Fasthttp server
func newBenchmarkServer() *BenchmarkServer {
	return &BenchmarkServer{
		Server: &fasthttp.Server{
			Name:                        "benchmark-server",
			Concurrency:                 256 * 1024,
			ReadBufferSize:             4096,
			WriteBufferSize:            4096,
			ReadTimeout:                30 * time.Second,
			WriteTimeout:               30 * time.Second,
			IdleTimeout:                120 * time.Second,
			MaxRequestBodySize:         16 * 1024 * 1024, // 16 MB
			MaxRequestBodySizeStream:  16 * 1024 * 1024, // 16 MB
			DisableKeepalive:          false,
			NoDefaultServerHeader:     true,
			DisableHeaderNormalizing: false,
			EnablePrintRoutes:         false,
			LogAllErrors:              false,
			SleepWhenConcurrencyLimits: 100 * time.Millisecond,
			MaxConnsPerIP:             0,
			MaxRequestsPerConn:        0,
			MaxIdleConns:              0,
			ResponseHeaderTimeout:    10 * time.Second,
			NoDefaultDate:            false,
			NoDefaultContentType:     false,
		},
	}
}

// addSecurityHeaders adds security headers to the response
func addSecurityHeaders(ctx *fasthttp.RequestCtx) {
	ctx.Response.Header.Set("X-Content-Type-Options", "nosniff")
	ctx.Response.Header.Set("X-Frame-Options", "DENY")
	ctx.Response.Header.Set("X-XSS-Protection", "1; mode=block")
	ctx.Response.Header.Set("Content-Security-Policy", "default-src 'self'")
	ctx.Response.Header.Set("Referrer-Policy", "strict-origin-when-cross-origin")
	ctx.Response.Header.Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
}

// mainHandler is the main request handler
func mainHandler(ctx *fasthttp.RequestCtx) {
	switch string(ctx.Method()) {
	case fasthttp.MethodGet:
		path := string(ctx.Path())
		if path == "/" {
			ctx.Response.Header.Set("Content-Type", "text/plain")
			addSecurityHeaders(ctx)
			ctx.SetStatusCode(fasthttp.StatusOK)
			return
		} else if bytes.HasPrefix(ctx.Path(), []byte("/user/")) {
			// Extract user ID from path
			id := string(ctx.Path()[6:]) // Skip "/user/"
			ctx.Response.Header.Set("Content-Type", "text/plain")
			addSecurityHeaders(ctx)
			ctx.SetStatusCode(fasthttp.StatusOK)
			ctx.SetBody([]byte(id))
			return
		} else if path == "/health" {
			ctx.Response.Header.Set("Content-Type", "text/plain")
			ctx.Response.Header.Set("Cache-Control", "no-cache, no-store, must-revalidate")
			addSecurityHeaders(ctx)
			ctx.SetStatusCode(fasthttp.StatusOK)
			ctx.SetBody([]byte("OK"))
			return
		}
	case fasthttp.MethodPost:
		path := string(ctx.Path())
		if path == "/user" {
			ctx.Response.Header.Set("Content-Type", "text/plain")
			addSecurityHeaders(ctx)
			ctx.SetStatusCode(fasthttp.StatusOK)
			return
		}
	}

	ctx.Response.Header.Set("Content-Type", "text/plain")
	addSecurityHeaders(ctx)
	ctx.SetStatusCode(fasthttp.StatusNotFound)
	ctx.SetBody([]byte("Not Found"))
}

func main() {
	// Create server instance
	server := newBenchmarkServer()

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port == "" {
		port = "3000"
	}

	// Configure server
	server.Server.Addr = ":" + port
	server.Server.Handler = mainHandler

	// Start server in a goroutine
	go func() {
		log.Printf("Starting Fasthttp benchmark server on port %s", port)
		if err := server.Server.ListenAndServe(); err != nil {
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
