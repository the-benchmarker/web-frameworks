package org.restheart;

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.plugins.ByteArrayService;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.plugins.RegisterPlugin.MATCH_POLICY;

@RegisterPlugin(name = "benchmarkRootService",
    description = "service for root resource",
    defaultURI = "/",
    uriMatchPolicy = MATCH_POLICY.EXACT,
    blocking = false)
public class RootService implements ByteArrayService {
  @Override
  public void handle(ByteArrayRequest request, ByteArrayResponse response) throws Exception {
    // RESTHeart handles several response headers including CORS headers and X-Powered-By
    // remove them reducing the response content lenght to ~1/3 for a fair competition :)
    response.getExchange().getResponseHeaders().clear();

    // nothing to do! this just sends 200 back
  }
}