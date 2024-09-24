package framework.benchmark.router;

import org.springframework.context.annotation.Bean;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;

import static org.springframework.web.reactive.function.server.RequestPredicates.GET;
import static org.springframework.web.reactive.function.server.RequestPredicates.POST;
import static org.springframework.web.reactive.function.server.RouterFunctions.route;

@Component
public class BenchmarkRouter {

    private static final Mono<ServerResponse> EMPTY_RESPONSE = ServerResponse.ok()
            .contentType(MediaType.TEXT_PLAIN)
            .body(Mono.empty(), String.class);

    @Bean
    public RouterFunction<ServerResponse> routes() {
        return route(GET("/"), this::root)
                .andRoute(GET("/user/{id}"), this::userId)
                .andRoute(POST("/user"), this::user);
    }

    // Reuse empty response to avoid constant re-creation
    public Mono<ServerResponse> root(ServerRequest request) {
        return EMPTY_RESPONSE;
    }

    public Mono<ServerResponse> userId(ServerRequest request) {
        // Directly parse the id from path variable without extra boxing/unboxing
        return ServerResponse.ok().bodyValue(Integer.parseInt(request.pathVariable("id")));
    }

    public Mono<ServerResponse> user(ServerRequest request) {
        return EMPTY_RESPONSE;
    }
}
