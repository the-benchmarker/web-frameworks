package benchmark.springboot;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
public class BenchmarkApplication {
  public static void main(String[] args) {
    SpringApplication.run(BenchmarkApplication.class, args);
  }

  @RestController
  public class BenchmarkController {
    @GetMapping("/")
    public void root() {}

    @GetMapping("/user/{id}")
    public Integer userId(@PathVariable Integer id) {
      return id;
    }

    @PostMapping("/user")
    public void user() {}
  }
}
