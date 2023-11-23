package web.helidon;

import io.helidon.webserver.http.HttpService;
import io.helidon.webserver.Routing;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;


public class IndexService implements HttpService {

    @Override
    public void routing(HttpRules rules) {
        rules.get("/", this::getDefaultMessageHandler);
    }

    private void getDefaultMessageHandler(ServerRequest request, ServerResponse response) {
        response.send("");
    }
}

