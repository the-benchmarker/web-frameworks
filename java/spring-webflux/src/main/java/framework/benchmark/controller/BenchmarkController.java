package framework.benchmark.controller;

import org.springframework.context.annotation.Bean;
import org.springframework.http.CacheControl;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;

import java.time.Duration;

import static org.springframework.web.reactive.function.server.RouterFunctions.route;

@Component
public class BenchmarkController {

    private static final CacheControl MAX_AGE_15_SECONDS = CacheControl.maxAge(Duration.ofSeconds(15));
    private static final Mono<ServerResponse> EMPTY_RESPONSE = ServerResponse.ok()
            .cacheControl(MAX_AGE_15_SECONDS)
            .headers(h -> {
                h.remove(HttpHeaders.DATE);
                h.remove(HttpHeaders.SERVER);
            })
            .build();


    @Bean
    public RouterFunction<ServerResponse> routes() {
        return route()
                .GET("/", request -> EMPTY_RESPONSE)
                .GET("/user/{id}", request -> ServerResponse.ok()
                        .cacheControl(MAX_AGE_15_SECONDS)
                        .headers(h -> {
                            h.remove(HttpHeaders.DATE);
                            h.remove(HttpHeaders.SERVER);
                        }).bodyValue(request.pathVariable("id")))
                .POST("/user", request -> EMPTY_RESPONSE)
                .build();
    }
}
