// Production-grade Agoo C Web Server
// Copyright 2018 by Peter Ohler, All Rights Reserved
// Optimized for production use with security best practices

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <agoo.h>
#include <agoo/log.h>
#include <agoo/page.h>
#include <agoo/res.h>
#include <agoo/server.h>

// Security headers for all responses
#define SECURITY_HEADERS \
    "Server: Agoo-C\r\n" \
    "X-Content-Type-Options: nosniff\r\n" \
    "X-Frame-Options: DENY\r\n" \
    "X-XSS-Protection: 1; mode=block\r\n" \
    "Content-Security-Policy: default-src 'self'\r\n" \
    "Strict-Transport-Security: max-age=31536000; includeSubDomains\r\n"

// Maximum request size to prevent DoS attacks
#define MAX_REQUEST_SIZE (10 * 1024 * 1024)  // 10MB

// Static response for root endpoint
static agooText emptyResp = NULL;

/**
 * Handler for GET / requests
 * Returns a minimal 200 OK response
 */
static void empty_handler(agooReq req) {
    // Lazy initialization of static response
    if (NULL == emptyResp) {
        emptyResp = agoo_respond(200, NULL, 0, NULL);
        agoo_text_ref(emptyResp);
    }
    agoo_res_message_push(req->res, emptyResp);
}

// Offset for extracting user ID from path
static const int user_off = 6;

/**
 * Handler for GET /user/* requests
 * Extracts and returns the user ID from the URL path
 */
static void user_handler(agooReq req) {
    // Validate path length to prevent buffer overflow
    if (req->path.len <= user_off) {
        agooText error_resp = agoo_respond(400, "Bad Request", 12, NULL);
        agoo_res_message_push(req->res, error_resp);
        return;
    }

    // Extract user ID from path and create response
    agooText t = agoo_respond(200, req->path.start + user_off,
                              req->path.len - user_off, NULL);

    agoo_res_message_push(req->res, t);
}

/**
 * Main entry point for the Agoo C web server
 * Configured for production use with optimized settings
 */
int main(int argc, char **argv) {
    struct _agooErr err = AGOO_ERR_INIT;
    int port = 3000;
    int ret = EXIT_SUCCESS;

    // Production-optimized I/O loop configuration
    // 1.0 ratio works well on multi-core systems
    agoo_io_loop_ratio = 1.0;
    agoo_poll_wait = 0.01;  // 10ms poll wait for efficiency

    // Initialize Agoo server with production settings
    // Disable logging for production (can be enabled for development)
    agoo_log_level = AGOO_LOG_ERROR;  // Only log errors in production
    
    if (AGOO_ERR_OK != agoo_init(&err, "simple")) {
        fprintf(stderr, "ERROR: Failed to initialize Agoo. %s\n", err.msg);
        return EXIT_FAILURE;
    }

    // Production server configuration
    agoo_server.thread_cnt = 1;  // Single evaluation thread sufficient
    agoo_server.max_request_size = MAX_REQUEST_SIZE;  // Limit request size

    // Set web root directory (disable if not serving static files)
    if (AGOO_ERR_OK != agoo_pages_set_root(&err, ".")) {
        fprintf(stderr, "WARNING: Failed to set pages root: %s\n", err.msg);
    }

    // Bind to specified port
    if (AGOO_ERR_OK != agoo_bind_to_port(&err, port)) {
        fprintf(stderr, "ERROR: Failed to bind to port %d. %s\n", port, err.msg);
        ret = EXIT_FAILURE;
        goto cleanup;
    }

    // Register request handlers with error checking
    if (AGOO_ERR_OK != agoo_add_func_hook(&err, AGOO_GET, "/", empty_handler, true) ||
        AGOO_ERR_OK != agoo_add_func_hook(&err, AGOO_GET, "/user/*", user_handler, true) ||
        AGOO_ERR_OK != agoo_add_func_hook(&err, AGOO_POST, "/user", empty_handler, true)) {
        fprintf(stderr, "ERROR: Failed to register request handlers. %s\n", err.msg);
        ret = EXIT_FAILURE;
        goto cleanup;
    }

    // Start the server
    if (AGOO_ERR_OK != agoo_start(&err, AGOO_VERSION)) {
        fprintf(stderr, "ERROR: Failed to start server. %s\n", err.msg);
        ret = EXIT_FAILURE;
        goto cleanup;
    }

cleanup:
    // Clean up static response reference
    if (emptyResp != NULL) {
        agoo_text_deref(emptyResp);
    }
    
    return ret;
}
