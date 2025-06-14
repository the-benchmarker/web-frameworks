package benchmark.springboot;

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RestController

@SpringBootApplication
class BenchmarkApplication

fun main(args: Array<String>) {
	runApplication<BenchmarkApplication>(*args)
}

@RestController
class BenchmarkController {
	@GetMapping("/")
	fun root() {}
	
	@GetMapping("/user/{id}")
	fun userId(@PathVariable id: Int): Int {
		return id;
	}
	
	@PostMapping("/user")
	fun user() {}
}
