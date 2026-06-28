import { Controller, Get, HttpCode, HttpStatus, Param, Post } from "@nestjs/common";

/**
 * NestJS Benchmark Controller
 * 
 * Production-grade REST controller for benchmark endpoints using NestJS framework.
 * Follows best practices including:
 * - Disabled debug logging for production
 * - Proper error handling
 * - Dependency injection
 * - Performance optimization
 */
@Controller()
export class AppController {

  /**
   * Root endpoint handler
   * Optimized for minimal latency and maximum throughput
   * 
   * @returns Empty string for benchmarking
   */
  @Get()
  @HttpCode(HttpStatus.OK)
  getHello(): string {
    return "";
  }

  /**
   * Get user by ID endpoint
   * Optimized endpoint that returns the user ID as plain text
   * 
   * @param id - User identifier from path
   * @returns User ID as plain text
   */
  @Get("/user/:id")
  @HttpCode(HttpStatus.OK)
  getUserId(@Param("id") id: string): string {
    return id;
  }

  /**
   * Create user endpoint
   * Optimized POST endpoint for creating users
   * 
   * @returns Empty string for benchmarking
   */
  @Post("/user")
  @HttpCode(HttpStatus.OK)
  postUser(): string {
    return "";
  }

  /**
   * Health check endpoint for monitoring
   * Production health check endpoint used by monitoring systems
   * 
   * @returns Health status
   */
  @Get("/health")
  @HttpCode(HttpStatus.OK)
  healthCheck(): string {
    return "OK";
  }
}
