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

    @Bean
    public RouterFunction<ServerResponse> routes() {
        return route()
                .GET("/", request -> ServerResponse.ok().bodyValue(""))
                .GET("/user/{id}", request -> ServerResponse.ok().bodyValue(request.pathVariable("id")))
                .POST("/user", request -> ServerResponse.ok().bodyValue(""))
                .build();
    }

}