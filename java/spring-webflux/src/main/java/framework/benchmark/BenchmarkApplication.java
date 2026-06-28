package framework.benchmark;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;

/**
 * Spring WebFlux Benchmark Application
 * 
 * <p>Production-grade reactive benchmark application following Spring Boot best practices:
 * - Reactive/non-blocking I/O
 * - Security enabled
 * - Minimal logging for production (WARN level)
 * - Error handling
 * - Caching support
 * - Virtual threads enabled</p>
 */
@SpringBootApplication
@EnableWebSecurity
@EnableCaching
public class BenchmarkApplication {

	/**
	 * Main application entry point.
	 * 
	 * <p>Runs the Spring Boot application with production-optimized settings.</p>
	 * 
	 * @param args Command line arguments
	 */
	public static void main(String[] args) {
		SpringApplication application = new SpringApplication(BenchmarkApplication.class);
		application.setAdditionalProfiles("prod");
		application.run(args);
	}

}
