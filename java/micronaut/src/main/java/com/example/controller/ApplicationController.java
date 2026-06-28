package com.example.controller;

import io.micronaut.core.annotation.NonBlocking;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Error;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.Post;
import io.micronaut.http.annotation.Produces;
import io.micronaut.http.hateoas.JsonError;
import jakarta.inject.Inject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Micronaut Benchmark Controller
 * 
 * <p>REST controller for benchmark endpoints using Micronaut framework.
 * Follows Micronaut best practices including dependency injection, logging,
 * and non-blocking I/O.</p>
 */
@Controller
@NonBlocking
public class ApplicationController {

    private static final Logger LOG = LoggerFactory.getLogger(ApplicationController.class);

    /**
     * Root endpoint handler.
     * 
     * @return Empty string for benchmarking
     */
    @Get(produces = MediaType.TEXT_PLAIN)
    public String index() {
        LOG.debug("Root endpoint accessed");
        return "";
    }

    /**
     * Get user by ID endpoint.
     * 
     * @param id The user identifier from path
     * @return The user ID as plain text
     */
    @Get(uri = "/user/{id}", produces = MediaType.TEXT_PLAIN)
    public String getUser(String id) {
        LOG.debug("User endpoint accessed with ID: {}", id);
        return id;
    }

    /**
     * Create user endpoint.
     * 
     * @return Empty string for benchmarking
     */
    @Post(uri = "/user", produces = MediaType.TEXT_PLAIN)
    public String createUser() {
        LOG.debug("Create user endpoint accessed");
        return "";
    }

    /**
     * Health check endpoint for monitoring.
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
     * @param ex The exception that was thrown
     * @return Error response
     */
    @Error(global = true)
    public JsonError handleException(Throwable ex) {
        LOG.error("Unhandled exception: {}", ex.getMessage(), ex);
        return new JsonError(ex.getMessage());
    }
}
