package framework.benchmark.controller;

import org.springframework.context.annotation.Bean;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;

import static org.springframework.web.reactive.function.server.RouterFunctions.route;

/**
 * Spring WebFlux Benchmark Controller
 * 
 * <p>Production-grade reactive REST controller for benchmark endpoints.
 * Follows best practices including:
 * - Reactive/non-blocking I/O
 * - Minimal logging for production (only errors)
 * - Proper error handling
 * - Optimized for benchmarking</p>
 */
@Component
public class BenchmarkController {

    /**
     * Configures the router function with benchmark endpoints.
     * 
     * <p>All endpoints are optimized for minimal latency and maximum throughput.
     * Returns plain text responses for benchmarking.</p>
     * 
     * @return The configured RouterFunction
     */
    @Bean
    public RouterFunction<ServerResponse> routes() {
        return route()
                .GET("/", request -> ServerResponse.ok().contentType(MediaType.TEXT_PLAIN).bodyValue(""))
                .GET("/user/{id}", request -> ServerResponse.ok().contentType(MediaType.TEXT_PLAIN).bodyValue(request.pathVariable("id")))
                .POST("/user", request -> ServerResponse.ok().contentType(MediaType.TEXT_PLAIN).bodyValue(""))
                .GET("/health", request -> ServerResponse.ok().contentType(MediaType.TEXT_PLAIN).bodyValue("OK"))
                .build();
    }

}