package benchmark.armeria;

import com.linecorp.armeria.common.ExchangeType;
import com.linecorp.armeria.common.HttpMethod;
import com.linecorp.armeria.common.HttpRequest;
import com.linecorp.armeria.common.HttpResponse;
import com.linecorp.armeria.server.HttpService;
import com.linecorp.armeria.server.Route;
import com.linecorp.armeria.server.RoutingContext;
import com.linecorp.armeria.server.Server;
import com.linecorp.armeria.server.ServiceRequestContext;

public class BenchmarkApplication {

    public static void main(String... args) {
        final Server server = Server.builder()
                .http(3000)
                .service("/", (UnaryService) (ctx, req) -> HttpResponse.of(""))
                .service("/user/:userId", (UnaryService) (ctx, req) -> HttpResponse.of(ctx.pathParam("userId")))
                .service(Route.builder()
                        .path("/user")
                        .methods(HttpMethod.POST)
                        .build(), (UnaryService) (ctx, req) -> HttpResponse.of(""))
                .build();

        server.closeOnJvmShutdown();
        server.start().join();
    }

    @FunctionalInterface
    private interface UnaryService extends HttpService {
        @Override
        default ExchangeType exchangeType(RoutingContext routingContext) {
            return ExchangeType.UNARY;
        }
    }
}
