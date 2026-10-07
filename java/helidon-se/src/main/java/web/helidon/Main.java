package web.helidon;

import io.helidon.config.Config;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.http.HttpRouting;

public class Main {
    private Main() {
    }

    public static void main(String[] args) {

        Config config = Config.create();
        WebServer server = WebServer.builder()
                .config(config.get("server"))
                .routing(Main::routing)
                .port(3000)
                .build()
                .start();

        System.out.println("WEB server is up! http://localhost:" + server.port() + "/");

    }


    static void routing(HttpRouting.Builder routing) {
        IndexService indexService = new IndexService();
        UserService userService = new UserService();

        routing
      		.register("/", indexService)
                .register("/user", userService);
    }
}
