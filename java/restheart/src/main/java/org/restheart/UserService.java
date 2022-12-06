package org.restheart;

/**
 *
 * @author Andrea Di Cesare <andrea@softinstigate.com>
 */

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.plugins.ByteArrayService;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.utils.HttpStatus;

@RegisterPlugin(name = "benchmarkUserService",
    description = "service for user resource",
    defaultURI = "/user",
    blocking = false)
public class UserService implements ByteArrayService {
  @Override
  public void handle(ByteArrayRequest request, ByteArrayResponse response) throws Exception {
    // RESTHeart handles several response headers including CORS headers and X-Powered-By
    // remove them reducing the response content lenght to ~1/3 for a fair competition :)
    response.getExchange().getResponseHeaders().clear();

    switch(request.getMethod()) {
        case POST -> { /* nothing to do! this just sends 200 back */ }
        case GET -> response.setContent(request.getPathParam("/user/{id}", "id"));
        default -> response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
    }
  }
}