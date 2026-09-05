/**
 * @file main.c
 * @brief the-benchmarker/web-frameworks contract app.
 *
 *   GET  /         -> 2xx, empty body
 *   GET  /user/:id -> 2xx, body is the id path parameter
 *   POST /user     -> 2xx, empty body
 */

#include <cwist/app.h>
#include <cwist/net/http/query.h>

static void empty(cwist_http_request *req, cwist_http_response *res) {
    (void)req;
    (void)res;
}

static void user_id(cwist_http_request *req, cwist_http_response *res) {
    const char *id = cwist_query_map_get(req->path_params, "id");
    if (id) cwist_sstring_assign(res->body, id);
}

int main(void) {
    cwist_app *app = cwist_app_create();
    cwist_app_get(app, "/", empty);
    cwist_app_get(app, "/user/:id", user_id);
    cwist_app_post(app, "/user", empty);
    cwist_app_listen(app, 3000);
    cwist_app_destroy(app);
    return 0;
}
