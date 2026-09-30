package benchmark.javalin;

import io.javalin.Javalin;


public class Bench {
  public static void main(String[] args) {
	  var app = Javalin.create(config -> {
    config.routes.get("/", ctx -> ctx.result(""));
    config.routes.get("/user/{id}", ctx -> ctx.result(ctx.pathParam("id")));
    config.routes.post("/user", ctx -> ctx.result(""));
        }).start(3000);
  }
}
