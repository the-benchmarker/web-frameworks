package com.example;

import com.hellokaton.blade.Blade;

public class Application {

public static void main(String[] args) {
        Blade.of()
          .get("/", ctx -> ctx.text(""))
          .get("/user/:id", ctx -> {
            ctx.text(ctx.pathString("id"));
          })
          .post("/user", ctx -> ctx.text(""))
          .start();
    
} }
