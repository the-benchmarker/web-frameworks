package io.github.suppierk.shoostr.benchmark;

import io.github.suppierk.shoostr.Options;
import io.github.suppierk.shoostr.Shoostr;

public final class Bench {
  private Bench() {}

  public static void main(String[] args) throws Exception {
    var options = new Options();
    options.setPort(3000);

    new Shoostr(options)
        .routes(
            routes -> {
              routes.get("/", (request, response) -> response.text(""));
              routes.get(
                  "/user/{id}",
                  (request, response) -> response.text(request.pathParam("id").orElseThrow()));
              routes.post("/user", (request, response) -> response.text(""));
            })
        .start();
  }
}
