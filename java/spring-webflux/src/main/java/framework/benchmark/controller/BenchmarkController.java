package framework.benchmark.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

@RestController
public class BenchmarkController {

    private static final String EMPTY = "";

    @GetMapping("/")
    public Mono<String> root() {
        return Mono.just(EMPTY);
    }

    @GetMapping("/user/{id}")
    public Mono<Integer> userId(@PathVariable Integer id) {
        return Mono.just(id);
    }

    @PostMapping("/user")
    public Mono<String> user() {
        return Mono.just(EMPTY);
    }
}
