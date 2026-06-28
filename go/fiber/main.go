package main

import (
	"log"
	"os"

	"github.com/gofiber/fiber/v3"
	"github.com/gofiber/fiber/v3/middleware/logger"
	"github.com/gofiber/fiber/v3/middleware/recover"
)

// BenchmarkServer represents the benchmark HTTP server using Fiber framework
type BenchmarkServer struct {
	App *fiber.App
}

func main() {
	// Create Fiber application with production configuration
	app := fiber.New(fiber.Config{
		CaseSensitive:             true,
		StrictRouting:             true,
		DisableHeaderNormalizing:  true,
		DisableStartupMessage:     false,
		EnablePrintRoutes:        false,
		BodyLimit:                16 * 1024 * 1024, // 16 MB
	})

	// Configure server
	server := &BenchmarkServer{
		App: app,
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
	log.Printf("Starting Fiber benchmark server on port %s", port)
	log.Fatal(app.Listen(":" + port))
}

// configureMiddleware configures Fiber middleware
func (s *BenchmarkServer) configureMiddleware() {
	// Recovery middleware for panic handling
	s.App.Use(recover.New(recover.Config{
		EnableStackTrace: true,
	}))

	// Logger middleware (skip health check for benchmarking)
	s.App.Use(logger.New(logger.Config{
		Format:     "${time} - ${status} - ${method} ${path}\n",
		TimeFormat: "2006-01-02 15:04:05",
		TimeInterval: 1,
		Filter: func(c *fiber.Ctx) bool {
			return c.Request().URI().Path() != "/health"
		},
	}))
}

// registerRoutes registers all benchmark endpoints
func (s *BenchmarkServer) registerRoutes() {
	// Root endpoint
	s.App.Get("/", s.rootHandler)

	// User endpoints
	s.App.Get("/user/:id", s.getUserHandler)
	s.App.Post("/user", s.createUserHandler)

	// Health check endpoint
	s.App.Get("/health", s.healthCheckHandler)
}

// rootHandler handles requests to the root endpoint
// @Summary Root endpoint
// @Description Root endpoint for benchmarking
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router / [get]
func (s *BenchmarkServer) rootHandler(c *fiber.Ctx) error {
	c.Set("Content-Type", "text/plain")
	return c.SendStatus(fiber.StatusOK)
}

// getUserHandler handles GET requests to /user/:id
// @Summary Get user by ID
// @Description Retrieve user information by ID
// @Produce plain
// @Param id path string true "User ID"
// @Success 200 {string} string "User ID"
// @Router /user/{id} [get]
func (s *BenchmarkServer) getUserHandler(c *fiber.Ctx) error {
	c.Set("Content-Type", "text/plain")
	return c.SendString(c.Params("id"))
}

// createUserHandler handles POST requests to /user
// @Summary Create user
// @Description Create a new user
// @Produce plain
// @Success 200 {string} string "Empty response"
// @Router /user [post]
func (s *BenchmarkServer) createUserHandler(c *fiber.Ctx) error {
	c.Set("Content-Type", "text/plain")
	return c.SendStatus(fiber.StatusOK)
}

// healthCheckHandler handles health check requests
// @Summary Health check
// @Description Health check endpoint for monitoring
// @Produce plain
// @Success 200 {string} string "OK"
// @Router /health [get]
func (s *BenchmarkServer) healthCheckHandler(c *fiber.Ctx) error {
	c.Set("Content-Type", "text/plain")
	return c.SendString("OK")
}
