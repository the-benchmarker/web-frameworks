package org.restheart;

/**
 *
 * @author Andrea Di Cesare <andrea@softinstigate.com>
 */

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.plugins.ByteArrayService;
import org.restheart.plugins.InterceptPoint;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.utils.HttpStatus;

@RegisterPlugin(name = "benchmarkUserService", description = "service for user resource", defaultURI = "/user", dontIntercept = {
    InterceptPoint.REQUEST_BEFORE_AUTH, InterceptPoint.RESPONSE, InterceptPoint.RESPONSE_ASYNC })
public class UserService implements ByteArrayService {
  @Override
  public void handle(ByteArrayRequest request, ByteArrayResponse response) throws Exception {
    if (request.isGet()) {
      response.setContent(request.getPathParam("/user/{id}", "id"));
    } else if (request.isPost()) {
      // nothing to do
    } else {
      response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
    }

    // clear cors headers for reduced response lenght
    response.getExchange().getResponseHeaders().clear();
  }
}