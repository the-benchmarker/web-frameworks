package org.restheart;

import static org.restheart.plugins.InterceptPoint.ANY;

import com.google.gson.JsonObject;
import org.restheart.exchange.JsonRequest;
import org.restheart.exchange.JsonResponse;
import org.restheart.exchange.Request;
import org.restheart.plugins.JsonService;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.utils.HttpStatus;

/**
 * Serves {@code GET /user/{id}} and {@code POST /user}.
 *
 * <p>The id is read from the decoded request path and must be exactly eight
 * ASCII digits; it is kept as a string so that leading zeros survive.</p>
 *
 * @author Andrea Di Cesare <andrea@softinstigate.com>
 */
@RegisterPlugin(
  name = "benchmarkUserService",
  description = "service for user resource",
  defaultURI = "/user",
  blocking = false,
  dontIntercept = ANY
)
public class UserService implements JsonService {

  private static final String PREFIX = "/user/";
  private static final int ID_LENGTH = 8;

  @Override
  public void handle(JsonRequest request, JsonResponse response) {
    switch (request.getMethod()) {
      case POST -> {
        if (!"/user".equals(request.getPath())) {
          response.setInError(HttpStatus.SC_NOT_FOUND, "not found");
        }
        // otherwise nothing to do: 200 with an empty body
      }
      case GET -> {
        var id = userId(request.getPath());
        if (id == null) {
          response.setInError(HttpStatus.SC_BAD_REQUEST, "invalid user id");
        } else {
          var body = new JsonObject();
          body.addProperty("id", id);
          response.setContent(body);
        }
      }
      default -> response.setInError(HttpStatus.SC_METHOD_NOT_ALLOWED, "method not allowed");
    }
  }

  /**
   * @param path the decoded request path
   * @return the eight-digit id, or null if the path does not carry a valid one
   */
  static String userId(String path) {
    if (path.length() != PREFIX.length() + ID_LENGTH || !path.startsWith(PREFIX)) {
      return null;
    }

    for (int i = PREFIX.length(); i < path.length(); i++) {
      char c = path.charAt(i);
      if (c < '0' || c > '9') {
        return null;
      }
    }

    return path.substring(PREFIX.length());
  }

  @Override
  public boolean corsEnabled(Request<?> r) {
    // disable CORS headers
    return false;
  }
}
