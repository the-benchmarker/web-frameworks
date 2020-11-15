package benchmark.restheart;

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.plugins.ByteArrayService;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.plugins.RegisterPlugin.MATCH_POLICY;
import org.restheart.utils.HttpStatus;

@RegisterPlugin(name = "benchmarkRootService", description = "service for root resource", defaultURI = "/", uriMatchPolicy = MATCH_POLICY.EXACT)
public class RootService implements ByteArrayService {
    @Override
    public void handle(ByteArrayRequest request, ByteArrayResponse response) throws Exception {
        if (!request.isGet()) {
            response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
        }
    }
}