package org.restheart;

import static org.restheart.plugins.InterceptPoint.ANY;

import org.restheart.exchange.ByteArrayRequest;
import org.restheart.exchange.ByteArrayResponse;
import org.restheart.exchange.Request;
import org.restheart.plugins.ByteArrayService;
import org.restheart.plugins.RegisterPlugin;
import org.restheart.utils.HttpStatus;

/**
 * User Service for RestHeart Benchmark
 *
 * <p>This service handles HTTP requests to user resources ("/user") and provides
 * user-specific endpoints for benchmarking purposes. It supports various HTTP methods
 * and extracts user IDs from URIs to test path parameter processing performance.</p>
 *
 * <p>The service supports the following operations:</p>
 * <ul>
 *   <li>GET /user/{userId} - Returns the user ID extracted from the URI</li>
 *   <li>POST /user - Accepts POST requests (returns 200 OK)</li>
 *   <li>OPTIONS /user - Handles OPTIONS requests</li>
 *   <li>Other methods return 501 Not Implemented</li>
 * </ul>
 *
 * @since 1.0.0
 * @author Andrea Di Cesare &lt;andrea@softinstigate.com&gt;
 * @author The Benchmarker Team
 */
@RegisterPlugin(
    name = "benchmarkUserService",
    description = "Benchmark service for user resource - handles user-specific requests",
    defaultURI = "/user",
    blocking = false,
    dontIntercept = ANY
)
public class UserService implements ByteArrayService {

    /**
     * The length of the user ID prefix in the URI ("/user/").
     * Used to extract the user ID from request paths.
     */
    private static final int USER_ID_PREFIX_LENGTH = "/user/".length();

    /**
     * Default constructor for UserService.
     * <p>Required by RestHeart plugin system.</p>
     */
    public UserService() {
        // Default constructor
    }

    /**
     * Handles incoming HTTP requests to user resources.
     *
     * <p>This method routes requests based on HTTP method and processes
     * user-specific operations for benchmarking purposes.</p>
     *
     * @param request  the incoming HTTP request containing method, path, and headers
     * @param response the HTTP response to be populated with status and content
     */
    @Override
    public void handle(ByteArrayRequest request, ByteArrayResponse response) {
        if (request == null || response == null) {
            response.setStatusCode(HttpStatus.SC_BAD_REQUEST);
            return;
        }

        switch (request.getMethod()) {
            case POST -> {
                // Accept POST requests and return 200 OK
                // No content processing for benchmarking purposes
                response.setStatusCode(HttpStatus.SC_OK);
            }
            case GET -> {
                String userId = extractUserId(request.getPath());
                if (userId != null && !userId.isEmpty()) {
                    response.setContent(userId);
                    response.setStatusCode(HttpStatus.SC_OK);
                } else {
                    // Return empty response for root /user path
                    response.setStatusCode(HttpStatus.SC_OK);
                }
            }
            case OPTIONS -> {
                handleOptions();
                response.setStatusCode(HttpStatus.SC_OK);
            }
            default -> {
                response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
            }
        }
    }

    /**
     * Extracts the user ID from the request path.
     *
     * <p>The user ID is expected to be the portion of the URI after "/user/".
     * For example, for the path "/user/john123", this returns "john123".
     * If the path is exactly "/user" or shorter, returns an empty string.</p>
     *
     * @param path the request URI path
     * @return the extracted user ID, or empty string if no user ID is present
     */
    static String extractUserId(String path) {
        if (path == null || path.length() <= USER_ID_PREFIX_LENGTH) {
            return "";
        }
        return path.substring(USER_ID_PREFIX_LENGTH);
    }

    /**
     * Handles OPTIONS HTTP method requests.
     *
     * <p>This method is called when an OPTIONS request is received.
     * Currently performs no action as CORS headers are disabled for performance.</p>
     */
    private void handleOptions() {
        // OPTIONS handling for benchmarking
        // No CORS headers are added for performance reasons
    }

    /**
     * Determines whether CORS headers should be added to responses.
     *
     * <p>CORS headers are disabled for performance benchmarking to
     * minimize response processing overhead.</p>
     *
     * @param request the incoming request (parameter name changed from 'r' for clarity)
     * @return false - CORS headers are disabled for performance
     */
    @Override
    public boolean corsEnabled(Request<?> request) {
        return false;
    }
}
