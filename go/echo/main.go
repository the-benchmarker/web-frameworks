package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
)

// BenchmarkServer represents the benchmark HTTP server using Echo framework
type BenchmarkServer struct {
	Echo   *echo.Echo
	Server *http.Server
}

// newBenchmarkServer creates a new production-ready Echo server
func newBenchmarkServer() *BenchmarkServer {
	// Create Echo instance with production configuration
	e := echo.New()
	e.HideBanner = true
	e.HidePort = true
	e.Debug = false

	return &BenchmarkServer{
		Echo: e,
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
	if port == "" {
		port = "3000"
	}

	// Create HTTP server with production-grade configuration
	server.Server = &http.Server{
		Addr:              ":" + port,
		Handler:          server.Echo,
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:      30 * time.Second,
		WriteTimeout:     30 * time.Second,
		IdleTimeout:      120 * time.Second,
		MaxHeaderBytes:   1 << 20, // 1 MB
	}

	// Start server in a goroutine for graceful shutdown
	go func() {
		log.Printf("Starting Echo benchmark server on port %s", port)
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

// configureMiddleware configures production-grade Echo middleware
func (s *BenchmarkServer) configureMiddleware() {
	// Recovery middleware for panic handling
	s.Echo.Use(middleware.RecoverWithConfig(middleware.RecoverConfig{
		LogErrorFunc: func(err error) error {
			log.Printf("PANIC: %v", err)
			return err
		},
		StackSize:         4 << 10, // 4 KB
		DisablePrintStack: false,
		DisableStackAll:  false,
	}))

	// Request ID middleware for tracing
	s.Echo.Use(middleware.RequestIDWithConfig(middleware.RequestIDConfig{
		Generator: func() string {
			return middleware.NewUUID().String()
		},
		RequestIDHeader: "X-Request-ID",
	}))

	// Real IP middleware for proper client IP detection
	s.Echo.Use(middleware.RealIPWithConfig(middleware.RealIPConfig{
		Skipper: middleware.DefaultSkipper,
		TrustIPs: []string{"10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16"},
	}))

	// Body limit middleware
	s.Echo.Use(middleware.BodyLimitWithConfig(middleware.BodyLimitConfig{
		Limit: "16M",
		OnExceedLimit: func(c echo.Context) error {
			return c.String(http.StatusRequestEntityTooLarge, "Request entity too large")
		},
	}))

	// Secure headers middleware with enhanced security
	s.Echo.Use(middleware.SecureWithConfig(middleware.SecureConfig{
		XFrameOptions:         "DENY",
		XContentTypeOptions:  "nosniff",
		XSSProtection:         "1; mode=block",
		ContentSecurityPolicy: "default-src 'self'",
		HSTSMaxAge:            31536000,
		HSTSIncludeSubdomains: true,
		HSTSPreload:           true,
	}))

	// Logger middleware (skip health check for benchmarking performance)
	s.Echo.Use(middleware.LoggerWithConfig(middleware.LoggerConfig{
		Format: `${time_rfc3339} - ${remote_ip} - ${method} ${path} ${status} ${latency_human}\n`,
		Skipper: func(c echo.Context) bool {
			return c.Request().URL.Path == "/health"
		},
		Output: os.Stdout,
	}))

	// CORS middleware (can be configured for production)
	s.Echo.Use(middleware.CORSWithConfig(middleware.CORSConfig{
		AllowOrigins: []string{"*"},
		AllowMethods: []string{echo.GET, echo.POST, echo.PUT, echo.DELETE, echo.PATCH, echo.HEAD, echo.OPTIONS},
		AllowHeaders: []string{echo.HeaderOrigin, echo.HeaderContentType, echo.HeaderAccept, echo.HeaderAuthorization},
		ExposeHeaders: []string{echo.HeaderContentType, echo.HeaderContentLength},
		MaxAge:       86400, // 24 hours
	}))

	// Custom error handler
	s.Echo.HTTPErrorHandler = func(err error, c echo.Context) {
		log.Printf("HTTP Error: %v", err)
		c.Response().Header().Set(echo.HeaderContentType, echo.MIMETextPlain)
		c.Response().Header().Set("X-Content-Type-Options", "nosniff")
		c.String(http.StatusInternalServerError, "Internal Server Error")
	}
}

// registerRoutes registers all benchmark endpoints
func (s *BenchmarkServer) registerRoutes() {
	// Root endpoint
	s.Echo.GET("/", s.rootHandler)

	// User endpoints
	s.Echo.GET("/user/:id", s.getUserHandler)
	s.Echo.POST("/user", s.createUserHandler)

	// Health check endpoint
	s.Echo.GET("/health", s.healthCheckHandler)

	// 404 handler
	s.Echo.RouteNotFound("/*", s.notFoundHandler)
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func (s *BenchmarkServer) rootHandler(c echo.Context) error {
	c.Response().Header().Set(echo.HeaderContentType, echo.MIMETextPlain)
	c.Response().Header().Set("X-Content-Type-Options", "nosniff")
	c.Response().Header().Set("X-Frame-Options", "DENY")
	c.Response().Header().Set("X-XSS-Protection", "1; mode=block")
	return c.String(http.StatusOK, "")
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func (s *BenchmarkServer) getUserHandler(c echo.Context) error {
	id := c.Param("id")
	c.Response().Header().Set(echo.HeaderContentType, echo.MIMETextPlain)
	c.Response().Header().Set("X-Content-Type-Options", "nosniff")
	c.Response().Header().Set("X-Frame-Options", "DENY")
	c.Response().Header().Set("X-XSS-Protection", "1; mode=block")
	return c.String(http.StatusOK, id)
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func (s *BenchmarkServer) createUserHandler(c echo.Context) error {
	c.Response().Header().Set(echo.HeaderContentType, echo.MIMETextPlain)
	c.Response().Header().Set("X-Content-Type-Options", "nosniff")
	c.Response().Header().Set("X-Frame-Options", "DENY")
	c.Response().Header().Set("X-XSS-Protection", "1; mode=block")
	return c.String(http.StatusOK, "")
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func (s *BenchmarkServer) healthCheckHandler(c echo.Context) error {
	c.Response().Header().Set(echo.HeaderContentType, echo.MIMETextPlain)
	c.Response().Header().Set("Cache-Control", "no-cache")
	c.Response().Header().Set("X-Content-Type-Options", "nosniff")
	c.Response().Header().Set("X-Frame-Options", "DENY")
	return c.String(http.StatusOK, "OK")
}

// notFoundHandler handles 404 Not Found
// @Summary Not Found
// @Description 404 handler
// @Produce plain
// @Success 404 {string} string "Not Found"
func (s *BenchmarkServer) notFoundHandler(c echo.Context) error {
	c.Response().Header().Set(echo.HeaderContentType, echo.MIMETextPlain)
	c.Response().Header().Set("X-Content-Type-Options", "nosniff")
	c.Response().Header().Set("X-Frame-Options", "DENY")
	return c.String(http.StatusNotFound, "Not Found")
}
