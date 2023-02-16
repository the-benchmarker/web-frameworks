package web.helidon;

import io.helidon.webserver.Service;
import io.helidon.webserver.Routing;

import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;


public class UserService implements Service {

    @Override
    public void update(Routing.Rules rules) {
        rules
                .post("/", this::getDefaultMessageHandler)
                .get("/{id}", this::getMessageHandler);
    }

    private void getMessageHandler(ServerRequest request, ServerResponse response) {
        String id = request.path().param("id");
        sendResponse(response, id);
    }

    private void sendResponse(ServerResponse response, String id) {
        response.send(id);
    }

    private void getDefaultMessageHandler(ServerRequest request, ServerResponse response) {
        response.send("");
    }
}