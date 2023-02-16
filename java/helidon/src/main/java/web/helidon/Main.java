package web.helidon;

import io.helidon.media.jsonp.JsonpSupport;
import io.helidon.common.reactive.Single;
import io.helidon.config.Config;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.Routing;

public final class Main {

    public static void main(final String[] args) {
        startServer();
    }

    static Single<WebServer> startServer() {

        Config config = Config.create();

        WebServer server = WebServer.builder(createRouting(config))
                .config(config.get("server"))
                .port(3000)
                .addMediaSupport(JsonpSupport.create())
                .build();

        Single<WebServer> webserver = server.start();
        return webserver;
    }

    private static Routing createRouting(Config config) {
        IndexService indexService = new IndexService();
        UserService userService = new UserService();
        Routing.Builder builder = Routing.builder()
                .register("/", indexService)
                .register("/user", userService);


        return builder.build();
    }
}