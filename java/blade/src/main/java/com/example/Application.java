package com.example;

import com.hellokaton.blade.Blade;

public class Application {
    public static void main(String[] args) {
        Blade.of()
            // GET /
            .get("/", ctx -> ctx.text(""))

            // GET /user/:id
            .get("/user/:id", ctx -> {
		    Integer id = ctx.pathInt("id");
                if (id == null) {
                    ctx.status(400).text("Bad Request: missing or invalid id");
                } else {
                    ctx.text(id.toString());
                }

            })

            // POST /user
            .post("/user", ctx -> ctx.text(""))

            // start app
            .start(Application.class, args);
    }
}

