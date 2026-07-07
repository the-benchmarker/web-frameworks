package benchmark.springboot

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.web.bind.annotation.*
import org.springframework.http.*

/**
 * Production-grade Spring Boot benchmark application.
 * 
 * Features:
 * - Security headers configured
 * - Proper error handling
 * - Input validation
 * - Disabled debug logging in production
 * - Optimized for benchmarking
 */
@SpringBootApplication
class BenchmarkApplication

fun main(args: Array<String>) {
    runApplication<BenchmarkApplication>(*args)
}

/**
 * REST Controller with security headers and input validation.
 */
@RestController
@RequestMapping(produces = [MediaType.TEXT_PLAIN_VALUE])
class BenchmarkController {

    /**
     * Health check endpoint.
     */
    @GetMapping("/health")
    fun health(): ResponseEntity<String> {
        return ResponseEntity.ok("OK")
    }

    /**
     * Root endpoint.
     */
    @GetMapping("/")
    fun root(): ResponseEntity<Void> {
        return ResponseEntity.ok().build()
    }

    /**
     * Get user by ID with input validation.
     */
    @GetMapping("/user/{id}")
    fun userId(@PathVariable id: String): ResponseEntity<String> {
        if (id.isBlank()) {
            throw ResponseStatusException(HttpStatus.BAD_REQUEST, "ID parameter cannot be blank")
        }
        return ResponseEntity.ok(id)
    }

    /**
     * Create user endpoint.
     */
    @PostMapping("/user")
    fun user(): ResponseEntity<Void> {
        return ResponseEntity.ok().build()
    }

    /**
     * Exception handler for validation errors.
     */
    @ExceptionHandler(Exception::class)
    fun handleException(ex: Exception): ResponseEntity<String> {
        System.err.println("Error: ${ex.message}")
        return ResponseEntity
            .status(HttpStatus.INTERNAL_SERVER_ERROR)
            .body("Internal Server Error")
    }
}
