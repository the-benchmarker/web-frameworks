package benchmark.armeria;

import com.linecorp.armeria.common.HttpMethod;
import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.server.Route;
import com.linecorp.armeria.server.Server;

public class BenchmarkApplication {

    private static final HttpResponse EMPTY_HTTP_RESPONSE = HttpResponse.of("");

    public static void main(String... args) {
        final Server server = Server.builder()
                .http(3000)
                .service("/", (ctx, req) -> EMPTY_HTTP_RESPONSE)
                .service("/user", (ctx, req) -> EMPTY_HTTP_RESPONSE)
                .service(Route.builder()
                        .path("/user/:userId")
                        .methods(HttpMethod.POST)
                        .build(), (ctx, req) -> HttpResponse.of(ctx.pathParam("userId")))
                .build();

        server.closeOnJvmShutdown();
        server.start().join();
    }
}
