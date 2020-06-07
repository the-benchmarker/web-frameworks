package com.example;

import com.blade.Blade;

public class Application {
  public static void main(String[] args) {
    Blade.of()
        .get("/", ctx -> ctx.text(""))
        .get("/user/:id", (request, response) -> { response.text(request.pathString("id")); })
        .post("/user", ctx -> ctx.text(""))
        .start(Application.class, args);
  }
}
