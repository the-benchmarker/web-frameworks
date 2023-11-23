package web.helidon;

// import io.helidon.media.jsonp.JsonpSupport;
// import io.helidon.common.reactive.Single;
import io.helidon.config.Config;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.http.HttpRouting;

public final class Main {

    public static void main(final String[] args) {

        Config config = Config.create();
        Config.global(config);

        WebServer server = WebServer.builder()
                .config(config.get("server"))
                .port(3000)
                .routing(Main::createRouting)
                .build()
                .start();
    }

    private static void createRouting(HttpRouting.Builder routing) {
        routing
            .register("/", new IndexService())
            .register("/user", new UserService());

    }
}