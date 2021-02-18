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
    defaultURI = "/user")
public class UserService implements ByteArrayService {
  @Override
  public void handle(ByteArrayRequest request, ByteArrayResponse response) throws Exception {
    // RESTHeart handles several response headers including CORS headers and X-Powered-By
    // remove them reducing the response content lenght to ~1/3 for a fairer competition :)
    response.getExchange().getResponseHeaders().clear();

    if (request.isPost()) {
      // nothing to do! this just sends 200 back
    } else if (request.isGet()) {
        response.setContent(request.getPathParam("/user/{id}", "id"));
    } else {
      response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
    }
  }
}