package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/gin-contrib/secure"
	"github.com/gin-gonic/gin"
)

// BenchmarkServer represents the benchmark HTTP server
// using Gin web framework with production-grade configuration
type BenchmarkServer struct {
	Router *gin.Engine
	Port   string
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready Gin server
func newBenchmarkServer() *BenchmarkServer {
	// Configure Gin for production
	gin.SetMode(gin.ReleaseMode)
	gin.DefaultWriter = os.Stdout
	gin.DisableConsoleColor()

	// Create router with production configuration
	router := gin.New()

	return &BenchmarkServer{
		Router: router,
		Port:   "3000",
	}
}

func main() {
	// Create server instance
	server := newBenchmarkServer()

	// Configure middleware
	server.configureMiddleware()

	// Register routes
	server.registerRoutes()

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port != "" {
		server.Port = port
	}

	// Create HTTP server with production-grade configuration
	server.Server = &http.Server{
		Addr:              ":" + server.Port,
		Handler:          server.Router,
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Start server in a goroutine for graceful shutdown
	go func() {
		log.Printf("Starting Gin benchmark server on port %s", server.Port)
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

// configureMiddleware configures production-grade middleware
func (s *BenchmarkServer) configureMiddleware() {
	// Recovery middleware for panic handling
	s.Router.Use(gin.Recovery())

	// Security middleware with production configuration
	s.Router.Use(secure.New(secure.Config{
		FrameDeny:          true,
		ContentTypeNosniff: true,
		BrowserXssFilter:    true,
		SSLRedirect:        false, // Set to true in production with HTTPS
		SSLTemporaryRedirect: false,
		STSSeconds:         31536000,
		STSIncludeSubdomains: true,
		STSPreload:         true,
		ForceSTSHeader:     false,
		ContentSecurityPolicy: "default-src 'self'",
	}))

	// Logger middleware (skip health check for benchmarking performance)
	s.Router.Use(gin.LoggerWithConfig(gin.LoggerConfig{
		Formatter: func(param gin.LogFormatterParams) string {
			return param.ClientIP + " - [" + param.TimeStamp.Format(time.RFC3339) + "] " +
				param.Method + " " + param.Path + " " + param.StatusCode + " " +
				param.Latency.String() + "\n"
		},
		SkipPaths: []string{"/health"},
		Output:    os.Stdout,
	}))

	// Rate limiting middleware (optional, can be enabled for production)
	// s.Router.Use(ratelimit.New(limiter))
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
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func (s *BenchmarkServer) rootHandler(c *gin.Context) {
	c.Header("Content-Type", "text/plain")
	c.Header("X-Content-Type-Options", "nosniff")
	c.Header("X-Frame-Options", "DENY")
	c.Header("X-XSS-Protection", "1; mode=block")
	c.Status(http.StatusOK)
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func (s *BenchmarkServer) getUserHandler(c *gin.Context) {
	id := c.Param("id")
	c.Header("Content-Type", "text/plain")
	c.Header("X-Content-Type-Options", "nosniff")
	c.Header("X-Frame-Options", "DENY")
	c.Header("X-XSS-Protection", "1; mode=block")
	c.String(http.StatusOK, id)
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func (s *BenchmarkServer) createUserHandler(c *gin.Context) {
	c.Header("Content-Type", "text/plain")
	c.Header("X-Content-Type-Options", "nosniff")
	c.Header("X-Frame-Options", "DENY")
	c.Header("X-XSS-Protection", "1; mode=block")
	c.Status(http.StatusOK)
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func (s *BenchmarkServer) healthCheckHandler(c *gin.Context) {
	c.Header("Content-Type", "text/plain")
	c.Header("Cache-Control", "no-cache")
	c.String(http.StatusOK, "OK")
}
