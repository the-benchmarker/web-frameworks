package org.restheart;

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.plugins.ByteArrayService;
import static org.restheart.plugins.InterceptPoint.ANY;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.plugins.RegisterPlugin.MATCH_POLICY;

@RegisterPlugin(name = "benchmarkRootService",
    description = "service for root resource",
    defaultURI = "/",
    uriMatchPolicy = MATCH_POLICY.EXACT,
    blocking = false,
    dontIntercept = ANY)
public class RootService implements ByteArrayService {
  @Override
  public void handle(ByteArrayRequest request, ByteArrayResponse response) throws Exception {
    // nothing to do! this just sends 200 back
  }
}