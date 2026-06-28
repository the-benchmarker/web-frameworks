package yolo;

import io.smallrye.common.annotation.NonBlocking;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

/**
 * Quarkus Benchmark Resource
 * 
 * <p>Production-grade JAX-RS resource for benchmark endpoints using Quarkus framework.
 * Follows best practices including:
 * - Proper REST design
 * - Non-blocking I/O
 * - Minimal logging for production (only errors)
 * - Optimized for benchmarking</p>
 */
@Path("/")
public class BenchmarkResource {

    /**
     * Root endpoint handler.
     * 
     * <p>Optimized for minimal latency and maximum throughput.
     * No logging in production for maximum performance.</p>
     * 
     * @return Empty string for benchmarking
     */
    @GET
    @Path("/")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
    public String root() {
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
    @GET
    @Path("/user/{id}")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
    public String userId(String id) {
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
    @POST
    @Path("/user")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
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
    @GET
    @Path("/health")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
    public String healthCheck() {
        return "OK";
    }
}