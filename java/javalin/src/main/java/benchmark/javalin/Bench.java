package benchmark.javalin;

import io.javalin.Javalin;

public class Bench {

    public static void main(String[] args) {
        Javalin app = Javalin.create().start(3000);
        app.get("/", ctx -> ctx.result(""));
        app.get("/user/:id", ctx -> ctx.result(ctx.pathParam("id")));
        app.post("/user", ctx -> ctx.result(""));
    }
}
