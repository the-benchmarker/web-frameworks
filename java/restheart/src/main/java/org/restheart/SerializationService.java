package org.restheart;

import static org.restheart.plugins.InterceptPoint.ANY;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import org.restheart.exchange.JsonRequest;
import org.restheart.exchange.JsonResponse;
import org.restheart.exchange.Request;
import org.restheart.plugins.JsonService;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.utils.HttpStatus;

/**
 * Serves {@code GET /serialization?seed={seed}}: builds and serializes, on every
 * request, a JSON array of 100 objects each containing only the given seed.
 *
 * <p>The raw query string is parsed here, before any decoding, so that repeated
 * names are detected even when percent-encoded (e.g. {@code seed=a&%73eed=b}).</p>
 *
 * @author Andrea Di Cesare <andrea@softinstigate.com>
 */
@RegisterPlugin(
  name = "benchmarkSerializationService",
  description = "JSON serialization benchmark",
  defaultURI = "/serialization",
  uriMatchPolicy = RegisterPlugin.MATCH_POLICY.EXACT,
  blocking = false,
  dontIntercept = ANY
)
public class SerializationService implements JsonService {

  private static final int SIZE = 100;
  private static final int SEED_LENGTH = 8;

  @Override
  public void handle(JsonRequest request, JsonResponse response) {
    if (!request.isGet()) {
      response.setInError(HttpStatus.SC_METHOD_NOT_ALLOWED, "method not allowed");
      return;
    }

    var seed = seed(request.getQueryString());

    if (seed == null) {
      response.setInError(HttpStatus.SC_BAD_REQUEST, "exactly one query parameter seed of eight lowercase letters is required");
      return;
    }

    var array = new JsonArray(SIZE);
    for (int i = 0; i < SIZE; i++) {
      var item = new JsonObject();
      item.addProperty("seed", seed);
      array.add(item);
    }

    response.setContent(array);
  }

  /**
   * @param qs the raw, undecoded query string
   * @return the seed, or null if the query string is not exactly one valid seed parameter
   */
  static String seed(String qs) {
    // more than one parameter (or an empty one) is never accepted
    if (qs == null || qs.isEmpty() || qs.indexOf('&') >= 0) {
      return null;
    }

    int eq = qs.indexOf('=');
    if (eq < 0) {
      return null;
    }

    var name = decode(qs.substring(0, eq));
    if (!"seed".equals(name)) {
      return null;
    }

    var value = decode(qs.substring(eq + 1));
    if (value == null || value.length() != SEED_LENGTH) {
      return null;
    }

    for (int i = 0; i < SEED_LENGTH; i++) {
      char c = value.charAt(i);
      if (c < 'a' || c > 'z') {
        return null;
      }
    }

    return value;
  }

  /**
   * @return the decoded component, or null if it is not valid percent-encoding
   */
  private static String decode(String s) {
    if (s.indexOf('%') < 0 && s.indexOf('+') < 0) {
      return s;
    }

    try {
      return URLDecoder.decode(s, StandardCharsets.UTF_8);
    } catch (IllegalArgumentException e) {
      return null;
    }
  }

  @Override
  public boolean corsEnabled(Request<?> r) {
    // disable CORS headers
    return false;
  }
}
