package org.restheart;

import static org.restheart.plugins.InterceptPoint.ANY;

/**
 *
 * @author Andrea Di Cesare <andrea@softinstigate.com>
 */

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.exchange.Request;
import org.restheart.plugins.ByteArrayService;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.utils.HttpStatus;

@RegisterPlugin(
  name = "benchmarkUserService",
  description = "service for user resource",
  defaultURI = "/user",
  blocking = false,
  dontIntercept = ANY
)
public class UserService implements ByteArrayService {
	private static final int URI_PREFIX_LENGTH = "/user".length();

  @Override
  public void handle(ByteArrayRequest request, ByteArrayResponse response) {
    switch (request.getMethod()) {
      case POST -> {
        /* nothing to do! this just sends 200 back */
      }
      case GET -> response.setContent(userName(request.getPath()));
      case OPTIONS -> handleOptions();
      default -> response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
    }
  }

  private static String userName(String path) {
    return path.substring(Math.min(path.length(), URI_PREFIX_LENGTH));
  }

  @Override
  public boolean corsEnabled(Request<?> r) {
    // disable CORS headers
    return false;
  }
}
