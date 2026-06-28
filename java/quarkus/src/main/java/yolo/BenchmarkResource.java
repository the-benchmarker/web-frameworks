package yolo;

import io.quarkus.logging.Log;
import io.smallrye.common.annotation.NonBlocking;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

/**
 * Quarkus Benchmark Resource
 * 
 * <p>JAX-RS resource for benchmark endpoints using Quarkus framework.
 * Follows Quarkus best practices including proper REST design, logging,
 * and non-blocking I/O.</p>
 */
@Path("/")
public class BenchmarkResource {

    /**
     * Root endpoint handler.
     * 
     * @return Empty string for benchmarking
     */
    @GET
    @Path("/")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
    public String root() {
        Log.debug("Root endpoint accessed");
        return "";
    }

    /**
     * Get user by ID endpoint.
     * 
     * @param id The user identifier from path
     * @return The user ID as plain text
     */
    @GET
    @Path("/user/{id}")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
    public String userId(String id) {
        Log.debugf("User endpoint accessed with ID: %s", id);
        return id;
    }

    /**
     * Create user endpoint.
     * 
     * @return Empty string for benchmarking
     */
    @POST
    @Path("/user")
    @Produces(MediaType.TEXT_PLAIN)
    @NonBlocking
    public String createUser() {
        Log.debug("Create user endpoint accessed");
        return "";
    }

    /**
     * Health check endpoint for monitoring.
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