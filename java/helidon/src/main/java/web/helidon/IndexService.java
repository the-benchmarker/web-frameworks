package web.helidon;

import io.helidon.webserver.Service;
import io.helidon.webserver.Routing;

import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;


public class IndexService implements Service {

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/", this::getDefaultMessageHandler);
    }

    private void getDefaultMessageHandler(ServerRequest request, ServerResponse response) {
        response.send("");
    }
}