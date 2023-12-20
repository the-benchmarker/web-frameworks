package benchmark.armeria;

import com.linecorp.armeria.common.HttpMethod;
import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.server.Route;
import com.linecorp.armeria.server.Server;

public class BenchmarkApplication {

    public static void main(String... args) {
        final Server server = Server.builder()
                .http(3000)
                .service("/", (ctx, req) -> HttpResponse.of(""))
                .service("/user/:userId", (ctx, req) -> HttpResponse.of(ctx.pathParam("userId")))
                .service(Route.builder()
                        .path("/user")
                        .methods(HttpMethod.POST)
                        .build(), (ctx, req) -> HttpResponse.of(""))
                .build();

        server.closeOnJvmShutdown();
        server.start().join();
    }
}
