package benchmark.springframework;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

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