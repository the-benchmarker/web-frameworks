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
public class BenchmarkController {

    private static final Mono<ServerResponse> EMPTY_RESPONSE = ServerResponse.ok().build();

    @Bean
    public RouterFunction<ServerResponse> routes() {
        return route()
                .GET("/", request -> EMPTY_RESPONSE)
                .GET("/user/{id}", request -> ServerResponse.ok().bodyValue(request.pathVariable("id")))
                .POST("/user", request -> EMPTY_RESPONSE)
                .build();
    }
}
