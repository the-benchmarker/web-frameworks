package com.example.controller;

import io.micronaut.core.annotation.NonBlocking;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Error;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.Post;
import io.micronaut.http.annotation.Produces;
import io.micronaut.http.hateoas.JsonError;
import jakarta.inject.Singleton;

/**
 * Micronaut Benchmark Controller
 * 
 * <p>Production-grade REST controller for benchmark endpoints using Micronaut framework.
 * Follows best practices including:
 * - Dependency injection
 * - Non-blocking I/O
 * - Minimal logging for production (only errors)
 * - Proper error handling
 * - Immutable response types</p>
 */
@Controller("/")
@Singleton
@NonBlocking
public class ApplicationController {

    /**
     * Root endpoint handler.
     * 
     * <p>Optimized for minimal latency and maximum throughput.
     * No logging in production for maximum performance.</p>
     * 
     * @return Empty string for benchmarking
     */
    @Get(produces = MediaType.TEXT_PLAIN)
    public String index() {
        return "";
    }

    /**
     * Get user by ID endpoint.
     * 
     * <p>Optimized endpoint that returns the user ID as plain text.
     * No logging in production for maximum performance.</p>
     * 
     * @param id The user identifier from path
     * @return The user ID as plain text
     */
    @Get(uri = "/user/{id}", produces = MediaType.TEXT_PLAIN)
    public String getUser(String id) {
        return id;
    }

    /**
     * Create user endpoint.
     * 
     * <p>Optimized POST endpoint for creating users.
     * Returns empty response for benchmarking.</p>
     * 
     * @return Empty string for benchmarking
     */
    @Post(uri = "/user", produces = MediaType.TEXT_PLAIN)
    public String createUser() {
        return "";
    }

    /**
     * Health check endpoint for monitoring.
     * 
     * <p>Production health check endpoint used by monitoring systems.
     * Always returns OK status.</p>
     * 
     * @return Health status
     */
    @Get(uri = "/health", produces = MediaType.TEXT_PLAIN)
    public String healthCheck() {
        return "OK";
    }

    /**
     * Global exception handler.
     * 
     * <p>Handles all uncaught exceptions and returns appropriate error responses.</p>
     * 
     * @param ex The exception that was thrown
     * @return Error response
     */
    @Error(global = true)
    public JsonError handleException(Throwable ex) {
        return new JsonError(ex.getMessage());
    }
}
