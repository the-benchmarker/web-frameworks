package org.restheart;

/**
 *
 * @author Andrea Di Cesare <andrea@softinstigate.com>
 */

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.plugins.ByteArrayService;
import static org.restheart.plugins.InterceptPoint.ANY;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.utils.HttpStatus;

@RegisterPlugin(name = "benchmarkUserService",
    description = "service for user resource",
    defaultURI = "/user",
    blocking = false,
    dontIntercept = ANY
    )
public class UserService implements ByteArrayService {
  @Override
  public void handle(ByteArrayRequest request, ByteArrayResponse response) throws Exception {
    switch(request.getMethod()) {
        case POST -> { /* nothing to do! this just sends 200 back */ }
        case GET -> response.setContent(request.getPathParam("/user/{id}", "id"));
        case OPTIONS -> handleOptions();
        default -> response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
    }
  }
}