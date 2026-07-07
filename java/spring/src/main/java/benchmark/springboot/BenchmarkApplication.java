package benchmark.springboot;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.http.MediaType;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Spring Boot Benchmark Application
 * 
 * <p>Production-grade benchmark application following Spring Boot best practices:
 * - Proper REST controller design
 * - Security enabled
 * - Minimal logging for production (WARN level)
 * - Error handling
 * - Caching support
 * - Virtual threads enabled</p>
 * 
 * @since 1.0.0
 */
@SpringBootApplication
@EnableWebSecurity
@EnableCaching
public class BenchmarkApplication {

    /**
     * Main application entry point.
     * 
     * <p>Runs the Spring Boot application with production-optimized settings.
     * Virtual threads are enabled for better performance.</p>
     * 
     * @param args Command line arguments
     */
    public static void main(String[] args) {
        SpringApplication application = new SpringApplication(BenchmarkApplication.class);
        application.setAdditionalProfiles("prod");
        application.run(args);
    }

    /**
     * REST Controller for benchmark endpoints.
     * 
     * <p>Provides optimized endpoints for benchmarking purposes.
     * All logging is disabled in production mode for maximum performance.</p>
     */
    @RestController
    @RequestMapping("/")
    @Tag(name = "Benchmark", description = "Production-grade benchmark endpoints")
    public static class BenchmarkController {

        /**
         * Root endpoint handler.
         * 
         * <p>Optimized for minimal latency and maximum throughput.</p>
         * 
         * @return Empty response for benchmarking
         */
        @GetMapping(value = "/", produces = MediaType.TEXT_PLAIN_VALUE)
        @Operation(summary = "Root endpoint", description = "Root endpoint for benchmarking")
        public String root() {
            return "";
        }

        /**
         * Get user by ID endpoint.
         * 
         * <p>Optimized endpoint that returns the user ID as plain text.
         * No logging in production for maximum performance.</p>
         * 
         * @param id The user identifier
         * @return The user ID as string for benchmarking
         */
        @GetMapping(value = "/user/{id}", produces = MediaType.TEXT_PLAIN_VALUE)
        @Operation(summary = "Get user by ID", description = "Retrieve user information by ID")
        public String userId(@PathVariable Integer id) {
            return id.toString();
        }

        /**
         * Create user endpoint.
         * 
         * <p>Optimized POST endpoint for creating users.
         * Returns empty response for benchmarking.</p>
         * 
         * @return Empty response for benchmarking
         */
        @PostMapping(value = "/user", produces = MediaType.TEXT_PLAIN_VALUE)
        @Operation(summary = "Create user", description = "Create a new user")
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
        @GetMapping(value = "/health", produces = MediaType.TEXT_PLAIN_VALUE)
        @Operation(hidden = true)
        public String healthCheck() {
            return "OK";
        }
    }
}
