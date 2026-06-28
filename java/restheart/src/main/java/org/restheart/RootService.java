package org.restheart;

import static org.restheart.plugins.InterceptPoint.ANY;

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.exchange.Request;
import org.restheart.plugins.ByteArrayService;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.plugins.RegisterPlugin.MATCH_POLICY;

/**
 * Root Service for RestHeart Benchmark
 *
 * <p>This service handles HTTP requests to the root resource ("/") and serves as
 * the entry point for the RestHeart benchmark application. It implements a
 * minimal handler that simply returns HTTP 200 OK responses to allow for
 * performance testing of the RestHeart framework overhead.</p>
 *
 * <p>The service is configured to:</p>
 * <ul>
 *   <li>Match exact root URI ("/")</li>
 *   <li>Execute asynchronously (non-blocking)</li>
 *   <li>Bypass all interceptors for minimal overhead</li>
 *   <li>Disable CORS headers for performance</li>
 * </ul>
 *
 * @since 1.0.0
 * @author The Benchmarker Team
 */
@RegisterPlugin(
    name = "benchmarkRootService",
    description = "Benchmark service for root resource - returns 200 OK for performance testing",
    defaultURI = "/",
    uriMatchPolicy = MATCH_POLICY.EXACT,
    blocking = false,
    dontIntercept = ANY
)
public class RootService implements ByteArrayService {

    /**
     * Default constructor for RootService.
     * <p>Required by RestHeart plugin system.</p>
     */
    public RootService() {
        // Default constructor
    }

    /**
     * Handles incoming HTTP requests to the root resource.
     *
     * <p>This implementation is intentionally minimal to provide accurate
     * performance measurements. It simply returns without modifying the
     * response, which results in a default 200 OK status code.</p>
     *
     * @param request  the incoming HTTP request
     * @param response the HTTP response to be sent back to the client
     */
    @Override
    public void handle(ByteArrayRequest request, ByteArrayResponse response) {
        // Intentionally minimal implementation for benchmarking purposes
        // The default behavior will return HTTP 200 OK
    }

    /**
     * Determines whether CORS headers should be added to responses.
     *
     * <p>CORS headers are disabled for performance benchmarking to
     * minimize response processing overhead.</p>
     *
     * @param request the incoming request
     * @return false - CORS headers are disabled for performance
     */
    @Override
    public boolean corsEnabled(Request<?> request) {
        return false;
    }
}
