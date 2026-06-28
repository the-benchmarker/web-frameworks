package main

import (
	"log"
	"net/http"
	"os"

	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
)

// BenchmarkServer represents the benchmark HTTP server using Echo framework
type BenchmarkServer struct {
	Echo *echo.Echo
}

func main() {
	// Create Echo instance with production configuration
	e := echo.New()
	
	// Configure server
	server := &BenchmarkServer{
		Echo: e,
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

	// Start server
	log.Printf("Starting Echo benchmark server on port %s", port)
	if err := e.Start(":" + port); err != nil {
		log.Fatalf("Failed to start server: %v", err)
	}
}

// configureMiddleware configures Echo middleware
func (s *BenchmarkServer) configureMiddleware() {
	// Recovery middleware for panic handling
	s.Echo.Use(middleware.RecoverWithConfig(middleware.RecoverConfig{
		LogErrorFunc: func(err error) error {
			log.Printf("PANIC: %v", err)
			return err
		},
	}))

	// Request ID middleware for tracing
	s.Echo.Use(middleware.RequestID())

	// Logger middleware (skip health check for benchmarking)
	s.Echo.Use(middleware.LoggerWithConfig(middleware.LoggerConfig{
		Skipper: func(c echo.Context) bool {
			return c.Request().URL.Path == "/health"
		},
	}))

	// Body limit middleware
	s.Echo.Use(middleware.BodyLimit("16M"))

	// Secure headers middleware
	s.Echo.Use(middleware.Secure())

	// Custom error handler
	s.Echo.HTTPErrorHandler = func(err error, c echo.Context) {
		log.Printf("HTTP Error: %v", err)
		c.Response().Header().Set(echo.HeaderContentType, echo.MIMETextPlain)
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
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func (s *BenchmarkServer) rootHandler(c echo.Context) error {
	c.Response().Header().Set(echo.HeaderContentType, echo.MIMETextPlain)
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
	return c.String(http.StatusOK, "OK")
}
