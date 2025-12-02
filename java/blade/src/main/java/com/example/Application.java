package com.example;

import com.hellokaton.blade.Blade;
import com.hellokaton.blade.mvc.RouteContext;

public class Application {
    public static void main(String[] args) {
        Blade.of()
            .get("/", Application::index)
            .get("/user/:id", Application::getUser)
            .post("/user", Application::createUser)
            .start(Application.class, args);
    }

    private static void index(RouteContext ctx) {
        ctx.text("");
    }

    private static void getUser(RouteContext ctx) {
        Integer id = ctx.pathInt("id");
        if (id == null) {
            ctx.status(400).text("Bad Request: missing or invalid id");
        } else {
            ctx.text(id.toString());
        }
    }

    private static void createUser(RouteContext ctx) {
        ctx.text("");
    }
}

