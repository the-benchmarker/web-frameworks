package main

import (
	"log"
	"net/http"
	"os"

	"github.com/gin-gonic/gin"
)

// BenchmarkServer represents the benchmark HTTP server
// using Gin web framework
var BenchmarkServer = &struct {
	Router *gin.Engine
	Port   string
}{
	Router: gin.New(),
	Port:   "3000",
}

func main() {
	// Configure Gin for production
	gin.SetMode(gin.ReleaseMode)
	gin.DefaultWriter = os.Stdout

	// Disable features not needed for benchmarking
	BenchmarkServer.Router.Use(gin.Recovery())
	BenchmarkServer.Router.Use(gin.LoggerWithConfig(gin.LoggerConfig{
		SkipPaths: []string{"/health"},
	}))

	// Register routes
	registerRoutes()

	// Get port from environment or use default
	port := os.Getenv("PORT")
	if port != "" {
		BenchmarkServer.Port = port
	}

	// Start server
	log.Printf("Starting Gin benchmark server on port %s", BenchmarkServer.Port)
	if err := BenchmarkServer.Router.Run(":" + BenchmarkServer.Port); err != nil {
		log.Fatalf("Failed to start server: %v", err)
	}
}

// registerRoutes registers all benchmark endpoints
func registerRoutes() {
	// Root endpoint
	BenchmarkServer.Router.GET("/", rootHandler)

	// User endpoints
	BenchmarkServer.Router.GET("/user/:id", getUserHandler)
	BenchmarkServer.Router.POST("/user", createUserHandler)

	// Health check endpoint
	BenchmarkServer.Router.GET("/health", healthCheckHandler)
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func rootHandler(c *gin.Context) {
	c.Writer.WriteHeader(http.StatusOK)
	c.Writer.Write([]byte(""))
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path int true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func getUserHandler(c *gin.Context) {
	id := c.Param("id")
	c.Writer.WriteHeader(http.StatusOK)
	c.Writer.Write([]byte(id))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func createUserHandler(c *gin.Context) {
	c.Writer.WriteHeader(http.StatusOK)
	c.Writer.Write([]byte(""))
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func healthCheckHandler(c *gin.Context) {
	c.Writer.WriteHeader(http.StatusOK)
	c.Writer.Write([]byte("OK"))
}
