#include "lwan.h"

/* Lwan routes a request to the handler with the longest matching prefix and
 * hands it the rest of the path in request->url; it has no named path
 * parameters. A handler has to set a MIME type, otherwise Lwan answers with
 * its default page for the returned status. */

LWAN_HANDLER_ROUTE(root, "/")
{
    if (request->url.len || lwan_request_get_method(request) != REQUEST_METHOD_GET)
        return HTTP_NOT_FOUND;

    response->mime_type = "text/plain";
    return HTTP_OK;
}

LWAN_HANDLER_ROUTE(user_id, "/user/")
{
    if (!request->url.len || lwan_request_get_method(request) != REQUEST_METHOD_GET)
        return HTTP_NOT_FOUND;

    response->mime_type = "text/plain";
    lwan_strbuf_set(response->buffer, request->url.value, request->url.len);
    return HTTP_OK;
}

LWAN_HANDLER_ROUTE(user, "/user")
{
    if (request->url.len || lwan_request_get_method(request) != REQUEST_METHOD_POST)
        return HTTP_NOT_FOUND;

    response->mime_type = "text/plain";
    return HTTP_OK;
}

int main(void)
{
    struct lwan_config config = *lwan_get_default_config();
    struct lwan l;

    config.listener = "0.0.0.0:3000";
    config.quiet = true;

    /* n_threads is left at 0: one I/O thread per online CPU. */
    lwan_init_with_config(&l, &config);
    lwan_detect_url_map(&l);
    lwan_main_loop(&l);
    lwan_shutdown(&l);

    return 0;
}
