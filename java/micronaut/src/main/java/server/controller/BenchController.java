package server.controller;

import io.micronaut.core.annotation.NonBlocking;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.Post;
import io.micronaut.http.annotation.Produces;

@Controller
@NonBlocking
public class BenchController {
  @Get(produces = MediaType.TEXT_PLAIN)
  public String index() {
    return "";
  }

  @Get(uri = "/user/{id}", produces = MediaType.TEXT_PLAIN)
  public String index(String id) {
    return id;
  }

  @Post(uri = "/user", consumes = MediaType.TEXT_PLAIN)
  public String post() {
    return "";
  }
}
