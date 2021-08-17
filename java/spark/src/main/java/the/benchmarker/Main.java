package the.benchmarker;

import static spark.Spark.*;

public class Main {
    public static void main(String[] args) {
        port(8080);
        get("/", (req, res) -> "");
        post("/user", (req, res) -> "");
        get("/user/:name", (request, response) -> {
            return request.params(":name");
        });
    }
}
