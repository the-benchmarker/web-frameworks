package benchmark.armeria;

import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.server.Server;

public class BenchmarkApplication {

    public static void main(String... args) {
        Server server = Server.builder()
                .http(8080)
                .service("/", (ctx, req) -> HttpResponse.of(""))
                .service("/user", (ctx, req) -> HttpResponse.of(""))
                .service("/user/:userId", (ctx, req) -> HttpResponse.of(ctx.pathParam("userId")))
                .build();

        server.closeOnJvmShutdown();
        server.start().join();
    }
}
