package org.restheart;

import static org.restheart.plugins.InterceptPoint.ANY;

import com.google.gson.JsonObject;
import io.undertow.server.HttpServerExchange;
import java.util.function.Consumer;
import java.util.function.Function;
import org.restheart.exchange.BadRequestException;
import org.restheart.exchange.JsonResponse;
import org.restheart.exchange.Request;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.plugins.Service;
import org.restheart.utils.HttpStatus;

/**
 * Serves {@code POST /deserialization}: parses the items of the JSON body and
 * returns their count and the sum of their ids.
 *
 * <p>Non-blocking: it runs on the IO thread, and RESTHeart reads the request
 * body for it without blocking before invoking it (RESTHeart 9.15). Parsing
 * 10 KB of JSON is shorter than the dispatch to a virtual thread.</p>
 *
 * <p>The media type is checked before the body is parsed, so an unsupported
 * media type returns 415 even when the body is malformed.</p>
 *
 * @author Andrea Di Cesare <andrea@softinstigate.com>
 */
@RegisterPlugin(
  name = "benchmarkDeserializationService",
  description = "JSON deserialization benchmark",
  defaultURI = "/deserialization",
  uriMatchPolicy = RegisterPlugin.MATCH_POLICY.EXACT,
  blocking = false,
  dontIntercept = ANY
)
public class DeserializationService implements Service<ItemsRequest, JsonResponse> {

  @Override
  public void handle(ItemsRequest request, JsonResponse response) {
    if (!request.isPost()) {
      response.setInError(HttpStatus.SC_METHOD_NOT_ALLOWED, "method not allowed");
      return;
    }

    if (!request.isContentTypeJson()) {
      response.setInError(HttpStatus.SC_UNSUPPORTED_MEDIA_TYPE, "a JSON request body is required");
      return;
    }

    var qs = request.getQueryString();
    if (qs != null && !qs.isEmpty()) {
      response.setInError(HttpStatus.SC_BAD_REQUEST, "query parameters are not accepted");
      return;
    }

    try {
      var summary = request.getContent();
      var body = new JsonObject();
      body.addProperty("count", summary.count());
      body.addProperty("checksum", summary.checksum());
      response.setContent(body);
    } catch (BadRequestException bre) {
      // getContent() already set the error response
    }
  }

  @Override
  public Consumer<HttpServerExchange> requestInitializer() {
    return ItemsRequest::init;
  }

  @Override
  public Consumer<HttpServerExchange> responseInitializer() {
    return JsonResponse::init;
  }

  @Override
  public Function<HttpServerExchange, ItemsRequest> request() {
    return ItemsRequest::of;
  }

  @Override
  public Function<HttpServerExchange, JsonResponse> response() {
    return JsonResponse::of;
  }

  @Override
  public boolean corsEnabled(Request<?> r) {
    // disable CORS headers
    return false;
  }
}
