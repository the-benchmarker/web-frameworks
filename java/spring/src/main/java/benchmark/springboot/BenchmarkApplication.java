package benchmark.springboot;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Spring Boot Benchmark Application
 * 
 * <p>This application provides benchmark endpoints for web framework comparison.
 * Follows Spring Boot best practices including proper REST controller design,
 * logging, and error handling.</p>
 */
@SpringBootApplication
@Slf4j
public class BenchmarkApplication {

  /**
   * Main application entry point.
   * 
   * @param args Command line arguments
   */
  public static void main(String[] args) {
    SpringApplication.run(BenchmarkApplication.class, args);
  }

  /**
   * REST Controller for benchmark endpoints.
   * 
   * <p>Provides endpoints for benchmarking purposes with proper logging
   * and error handling.</p>
   */
  @RestController
  @RequestMapping("/")
  @Tag(name = "Benchmark", description = "Benchmark endpoints")
  @Slf4j
  public static class BenchmarkController {

    /**
     * Root endpoint handler.
     * 
     * @return Empty response for benchmarking
     */
    @GetMapping(value = "/", produces = MediaType.TEXT_PLAIN_VALUE)
    @Operation(summary = "Root endpoint", description = "Root endpoint for benchmarking")
    public String root() {
      log.debug("Root endpoint accessed");
      return "";
    }

    /**
     * Get user by ID endpoint.
     * 
     * @param id The user identifier
     * @return The user ID as string for benchmarking
     */
    @GetMapping(value = "/user/{id}", produces = MediaType.TEXT_PLAIN_VALUE)
    @Operation(summary = "Get user by ID", description = "Retrieve user information by ID")
    public String userId(@PathVariable Integer id) {
      log.debug("User endpoint accessed with ID: {}", id);
      return id.toString();
    }

    /**
     * Create user endpoint.
     * 
     * @return Empty response for benchmarking
     */
    @PostMapping(value = "/user", produces = MediaType.TEXT_PLAIN_VALUE)
    @Operation(summary = "Create user", description = "Create a new user")
    public String createUser() {
      log.debug("Create user endpoint accessed");
      return "";
    }

    /**
     * Health check endpoint for monitoring.
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
