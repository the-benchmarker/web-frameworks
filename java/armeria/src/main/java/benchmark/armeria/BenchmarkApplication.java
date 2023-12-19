package benchmark.armeria;

import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.server.Server;

public class BenchmarkApplication {

    private static final HttpResponse EMPTY_HTTP_RESPONSE = HttpResponse.of("");

    public static void main(String... args) {
        Server server = Server.builder()
                .http(3000)
                .service("/", (ctx, req) -> EMPTY_HTTP_RESPONSE)
                .service("/user", (ctx, req) -> EMPTY_HTTP_RESPONSE)
                .service("/user/:userId", (ctx, req) -> HttpResponse.of(ctx.pathParam("userId")))
                .build();

        server.closeOnJvmShutdown();
        server.start().join();
    }
}
