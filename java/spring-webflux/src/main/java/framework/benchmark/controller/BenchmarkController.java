package framework.benchmark.controller;

import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;

import static org.springframework.web.reactive.function.server.RouterFunctions.route;

@Component
public class BenchmarkController {

    private static final Mono<ServerResponse> EMPTY_RESPONSE = ServerResponse.ok()
            .contentType(MediaType.TEXT_PLAIN)
            .headers(h -> {
                h.remove(HttpHeaders.SERVER);
                h.remove(HttpHeaders.CONNECTION);
            })
            .build().share();


    @Bean
    public RouterFunction<ServerResponse> routes() {
        return route()
                .GET("/", request -> EMPTY_RESPONSE)
                .GET("/user/{id}", request -> ServerResponse.ok()
                        .contentType(MediaType.TEXT_PLAIN)
                        .headers(h -> {
                            h.remove(HttpHeaders.SERVER);
                            h.remove(HttpHeaders.CONNECTION);
                        })
                        .bodyValue(request.pathVariable("id")))
                .POST("/user", request -> EMPTY_RESPONSE)
                .build();
    }

}
