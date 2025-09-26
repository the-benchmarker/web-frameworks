package com.example;

import com.hellokaton.blade.Blade;

public class Application {
    public static void main(String[] args) {
        Blade.create()
            .get("/", ctx -> {
                ctx.text("");
            })
            .get("/user/:id", ctx -> {
		Integer id = ctx.pathInt("id");
		if (id == null) {
			ctx.status(400);
		} else {
			ctx.text(id.toString());
		}
            })
            .post("/user", ctx -> {
                ctx.text("");
            })
            .start(Application.class, args);
    }
}

