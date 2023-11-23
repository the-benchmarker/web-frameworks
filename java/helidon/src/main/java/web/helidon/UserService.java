package web.helidon;

import io.helidon.webserver.Routing;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.HttpService;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;


public class UserService implements HttpService {

    @Override
    public void routing(HttpRules rules) {
        rules
                .post("/", this::getDefaultMessageHandler)
                .get("/{id}", this::getMessageHandler);
    }

    private void getMessageHandler(ServerRequest request, ServerResponse response) {
        String id = request.path().pathParameters().get("id");
        sendResponse(response, id);
    }

    private void sendResponse(ServerResponse response, String id) {
        response.send(id);
    }

    private void getDefaultMessageHandler(ServerRequest request, ServerResponse response) {
        response.send("");
    }
}