import { Controller, Get, HttpCode, HttpException, HttpStatus, Param, Post, Res } from "@nestjs/common";
import { Logger } from "@nestjs/common";
import { Response } from "express";

/**
 * NestJS Benchmark Controller
 * 
 * REST controller for benchmark endpoints using NestJS framework.
 * Follows NestJS best practices including proper error handling, logging,
 * and dependency injection.
 */
@Controller()
export class AppController {
  private readonly logger = new Logger(AppController.name);

  /**
   * Root endpoint handler
   * 
   * @returns Empty string for benchmarking
   */
  @Get()
  @HttpCode(HttpStatus.OK)
  getHello(): string {
    this.logger.debug("Root endpoint accessed");
    return "";
  }

  /**
   * Get user by ID endpoint
   * 
   * @param id - User identifier from path
   * @returns User ID as plain text
   */
  @Get("/user/:id")
  @HttpCode(HttpStatus.OK)
  getUserId(@Param("id") id: string): string {
    this.logger.debug(`User endpoint accessed with ID: ${id}`);
    return id;
  }

  /**
   * Create user endpoint
   * 
   * @returns Empty string for benchmarking
   */
  @Post("/user")
  @HttpCode(HttpStatus.OK)
  postUser(): string {
    this.logger.debug("Create user endpoint accessed");
    return "";
  }

  /**
   * Health check endpoint for monitoring
   * 
   * @returns Health status
   */
  @Get("/health")
  @HttpCode(HttpStatus.OK)
  healthCheck(): string {
    return "OK";
  }
}
