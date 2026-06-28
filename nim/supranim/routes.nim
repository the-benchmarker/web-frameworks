# Production-grade Supranim routes
# Security best practices, performance optimizations, and clean code
#
# This file is automatically imported by the Supranim framework.
# It is used to define the routes for the application.

routes:
  # Health check endpoint
  get "/":
    # GET route links to `getHomepage` controller
    # Security: Only accessible via GET method
    
  # User endpoint with type-safe parameter
  get "/user/{id:IdParam}":
    # GET route with dynamic parameter `id`
    # Security: Type-safe parameter validation
    # Performance: Optimized routing for benchmarking
    
  # User creation endpoint
  post "/user":
    # POST route for creating a new user
    # Security: Only accessible via POST method
    # Performance: Minimal processing for benchmarking