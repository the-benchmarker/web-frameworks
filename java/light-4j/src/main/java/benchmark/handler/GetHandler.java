package benchmark.handler;

import com.networknt.handler.LightHttpHandler;
import io.undertow.server.HttpServerExchange;

public class GetHandler implements LightHttpHandler {
    
    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        exchange.endExchange();
    }
}
