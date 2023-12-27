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
import com.linecorp.armeria.server.annotation.Get;
import com.linecorp.armeria.server.annotation.Param;
import com.linecorp.armeria.server.annotation.Post;

public class BenchmarkApplication {

    public static void main(String... args) {
        final Server server = Server.builder()
                .http(3000)
                .annotatedService(new UserService())
                .build();

        server.closeOnJvmShutdown();
        server.start().join();
    }

    private static final class UserService {
        @Get("/")
        public String index() {
            return "";
        }

        @Get("/user/:userId")
        public String getUser(@Param("userId") String userId) {
            return userId;
        }

        @Post("/user")
        public String createUser() {
            return "";
        }
    }
}
