package benchmark.handler;

import com.networknt.handler.LightHttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.HttpString;

public class UserIdGetHandler implements LightHttpHandler {
    
    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        exchange.getResponseHeaders().add(new HttpString("Content-Type"), "text/plain");
        exchange.setStatusCode(200);
        exchange.getResponseSender().send(exchange.getPathParameters().get("id").getFirst());
    }
}
